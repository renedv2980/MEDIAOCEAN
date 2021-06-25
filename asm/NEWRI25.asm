*          DATA SET NEWRI25    AT LEVEL 086 AS OF 05/01/02                      
*PHASE T32025A,+0                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T32025 - N6  REPORT  PHASE'                                     
T32025   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**N6WR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T32025,RB,RA      RA = 2ND BASE REG                              
         L     R8,ASPOOLD        * ANETWS1   = OPEN WORK AREAS                  
         USING SPOOLD,R8                                                        
         L     R9,ASYSD          * ANETWS2 = FIXED WORK AREA                    
         USING NETSYSD,R9                                                       
         L     R6,ANETWS4        * ANETWS3 = CLIST                              
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         L     R7,ANETWS2        * ANETWS4 = NDDEMBLK                           
         USING MYD,R7                                                           
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         LA    RE,WORKAREA                                                      
         L     RF,=F'50000'                                                     
         XCEF                                                                   
*                                                                               
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BAS   RE,REPMOD                                                        
XIT      XIT1                                                                   
         EJECT                                                                  
         SPACE 3                                                                
REPMOD   NTR1                                                                   
*                            REPORT INITIALIZATION                              
* GET NETGOAL ADDRS                                                             
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB             PICK UP ADDRESS OF NETGOAL                   
         ST    RF,ANETGOAL                                                      
* GET DEMOCON ADDRS                                                             
         GOTO1 NBCALLOV,DMCB,0,X'D9000AE0'                                      
         L     RF,DMCB             PICK UP ADDRESS OF DEMOCON                   
         ST    RF,ADEMOCON                                                      
         SPACE                                                                  
* SET UP SORTREC                                                                
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         B     REP1                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,4,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=50'                                    
*                                                                               
REP1     DS    0H                   GET WEEKLIST                                
WKLOOP   NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALDAT                                                  
         BNE   WKLOOP                                                           
         MVI   PERTYPE,C'W'        WEEKS ONLY                                   
         LA    R3,53               FOR 53 WEEKS MAX                             
         ST    R3,NUMPER                                                        
         L     R3,ANETWS1                                                       
         NETGO NVWKLST,DMCB,NUMPER,(R3),PERTYPE                                 
**                                                                              
** DO NOT USE WEEKLIST, BUT NEED ROUTINE TO SET UP NBCMPSTRT/END                
** AND ANYTHING ELSE IT MIGHT DO - COMPATIBLE WITH OTHER GOAL PROG              
         EJECT                                                                  
*                                                                               
         L     R5,ANETWS1                GOAL RECS                              
         USING NETGOALD,R5                                                      
         XC    0(150,R5),0(R5)                                                  
         LA    R2,NETBLOCK                                                      
         ST    R2,NGANTBLK                                                      
         L     R2,ANETWS1                                                       
         LA    R2,150(R2)                                                       
         ST    R2,NGAPLIST                                                      
         MVI   NGMAXPRD,100        LIST=100 PRODS MAX                           
         LA    R2,NETGOLHK                                                      
         ST    R2,NGAHOOK                                                       
         LA    R2,MYWORK                                                        
         USING SRTRECD,R2                                                       
         XC    0(SRECLENE,R2),0(R2)                                             
         GOTO1 ANETGOAL,DMCB,NGBLOCK                                            
         B     REP5                                                             
*                                                                               
NETGOLHK NTR1                                                                   
         SPACE                                                                  
         MVC   SDPT,NGOALDP        DAYPART                                      
         MVC   SPRD,NGOALPRD       PRODUCT CODE                                 
         MVC   SGOAL,NGOALTRG      GOAL TARGET DEMO                             
         CLI   SGOAL,1             IS IT HOMES                                  
         BNE   *+10                   NO/SKIP GRP                               
         MVC   SGGRP,NGOALGRP      GOAL GRP                                     
         MVC   SGDOL,NGOALDOL      GOAL DOLLARS                                 
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         B     XIT                                                              
         DROP  R5,R2                                                            
         EJECT                                                                  
*                                                                               
REP5     DS    0H                                                               
         MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POOL                 
         MVI   NBHUNOPT,C'Y'                                                    
         NETGO NVDEMOPT,DMCB       ACT SCHED/EST DEMO BUT NOT FOR PFB           
GETUNIT  LA    R2,MYWORK                                                        
         USING SRTRECD,R2                                                       
         XC    0(SRECLENE,R2),0(R2)                                             
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    REP7                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    REP20                                                            
         B     GETUNIT                                                          
*                                                                               
REP7     CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CLTSV,0             SET CLTSV FOR PRGPAGE                        
         BNE   *+10                                                             
         MVC   CLTSV,NBACTCLI                                                   
         L     RE,NBACTUAL              ACTUAL DOLLARS                          
         SRDA  RE,32(0)                                                         
         D     RE,=F'100'                                                       
         STCM  RF,15,SACTDOL                                                    
         BAS   RE,PRDCD                                                         
         MVC   SPRD,WORK                 PRODUCT                                
         MVC   SDPT,NBACTDP              DAYPART                                
         MVC   S1DEME+2(2),NDESTDEM+2                                           
         MVC   S1IMPE,NDESTDEM+4                                                
         MVC   S1DEMA+2(2),NDACTDEM+2                                           
         MVC   S1IMPA,NDACTDEM+4                                                
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         B     GETUNIT                                                          
         EJECT                                                                  
**************************************************                              
*  GET RECORDS FROM SORTER                                                      
*                                                                               
*                                                                               
REP20    DS    0H                                                               
         L     R3,ANETWS1                                                       
         USING SRTRECD,R3                                                       
REP21    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BNZ   REP22                                                            
         CLI   FRST,C'N'                                                        
         BE    REP50               END OF SORT                                  
         LA    R2,P1                                                            
         MVC   0(18,R2),=C'NO DATA TO PROCESS'                                  
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
*                                                                               
REP22    DS    0H                                                               
         CLI   FRST,C'N'                                                        
         BE    REP23                                                            
         MVI   FRST,C'N'                                                        
         MVC   FULL(3),SPRD                                                     
         BAS   RE,ROUT1                                                         
REP23    DS    0H                                                               
         CLC   SDPT,CURDPT         CHK DPT CHANGE                               
         BE    REP24                                                            
         BAS   RE,PRDLINE                                                       
         BAS   RE,DPTTOT                                                        
         MVC   FULL(3),SPRD                                                     
         BAS   RE,ROUT1                                                         
         B     REP30                                                            
REP24    DS    0H                                                               
         CLC   CURPRD,SPRD         CHK PROD CHANGE                              
         BE    REP30                                                            
         BAS   RE,PRDLINE                                                       
         MVC   FULL(3),SPRD                                                     
         BAS   RE,GETPRDNM                                                      
*                                                                               
REP30    BAS   RE,PROCREC                                                       
         B     REP21                                                            
*                                                                               
REP50    DS    0H                  END OF SORTRECS                              
         LA    R2,MYWORK                                                        
         OC    0(WRECLENE,R2),0(R2)                                             
         BZ    REP55                                                            
         BAS   RE,PRDLINE                                                       
         BAS   RE,DPTTOT                                                        
REP55    DS    0H                                                               
         BAS   RE,ALLDPTS                                                       
         MVI   SKIPHD,C'Y'                                                      
         BAS   RE,PRINTIT          SKIP LINE BEFORE FINAL TOTS                  
         BAS   RE,FNLTOTS                                                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************                                 
* SORTRECS ARE COMBINED IN WORK AREA                                            
* THIS WORKAREA IS FLUSHED AT CHANGE OF PROD/DPT                                
*                                                                               
PROCREC  NTR1                                                                   
         LA    RF,MYWORK                                                        
         USING WRKRECD,RF                                                       
         USING SRTRECD,R3                                                       
         LA    RF,WGDOL                                                         
         LA    R2,SGDOL                                                         
         BAS   RE,ROLLEM                                                        
PRCX     B     XIT                                                              
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************                                             
* FLUSHES WORKREC TO PRINT PROD LINE FOR A DAYPART                              
*                                                                               
PRDLINE  NTR1                                                                   
         LA    R2,MYWORK                                                        
         USING WRKRECD,R2                                                       
         ST    R2,ACUR1            STORE R2                                     
         MVI   WTYPE,0                                                          
         OC    WGGRP,WGGRP         IF GOAL GRPS MAKE IT B TYPE                  
         BZ    *+8                                                              
         MVI   WTYPE,C'B'                                                       
*                                                                               
         LA    R1,P                                                             
         USING PLINED,R1                                                        
         MVC   PDPT+3(20),PRODNAME                                              
         LA    R2,WGDOL                                                         
         BAS   RE,FLUSH                PRINT LINE                               
*                                                                               
         LA    RF,DPTGDOL                                                       
         BAS   RE,ROLLEM               ROLL TO DAYPART TOTAL                    
         L     R2,ACUR1            RESTORE R2                                   
         CLI   WTYPE,0                                                          
         BNE   PRD2                    ROLL TO SUBTOTA/B                        
         LA    R2,WGDOL                                                         
         LA    RF,SUBTOTA                                                       
         BAS   RE,ROLLEM                                                        
         B     *+16                                                             
PRD2     LA    RF,SUBTOTB                                                       
         LA    R2,WGDOL                                                         
         BAS   RE,ROLLEM                                                        
*                                                                               
         LA    R3,WORKAREA         ROLL TO COMBINED DAYPART BY PROD             
PRD3     OC    0(DPTRECLN,R3),0(R3)                                             
         BZ    PRD5                                                             
         CLC   PRODNAME,0(R3)                                                   
         BE    PRD5                                                             
         LA    R3,DPTRECLN(R3)                                                  
         B     PRD3                                                             
PRD5     MVC   0(20,R3),PRODNAME                  MATCH(OR NEW PROD)            
         LA    RF,20(R3)                                                        
         BAS   RE,ROLLEM                                                        
*                                                                               
         L     R2,ACUR1                RESTORE R2                               
         XC    0(WRECLENE,R2),0(R2)    CLEAR AREA                               
*                                                                               
         B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************                                             
* PRINT TOTAL LINE FOR A DAYPART                                                
*                                                                               
DPTTOT   NTR1                                                                   
         LA    R1,P                                                             
         USING PLINED,R1                                                        
         MVC   PDPT+3(5),=C'TOTAL'                                              
         LA    R2,DPTGDOL                                                       
         BAS   RE,FLUSH            PRINT DAYPART TOTAL                          
         MVC   H1SAVE,P                                                         
         BAS   RE,PRINTIT          SKIP LINE                                    
         XC    DPTOTAL(DPTRECLN),DPTOTAL                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************                                             
* PRINTS COMBINED DAYPART TOTALS BY PRODUCT                                     
*                                                                               
ALLDPTS  NTR1                                                                   
         BAS   RE,PRINTIT          SKIP LINE                                    
         LA    R1,P                                                             
         USING PLINED,R1                                                        
         MVC   PDPT(17),=C'COMBINED DAYPARTS'                                   
         MVC   PDPT+132(17),=C'-----------------'                               
         MVC   PDPT+18(6),=C'(CONT)'                                            
         MVC   H1SAVE,P                                                         
         XC    PDPT+18(6),PDPT+18                                               
         MVC   H2SAVE,P2                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
         LA    R4,WORKAREA                                                      
ALLD5    MVC   PDPT+3(20),0(R4)                                                 
         LA    R2,20(R4)                                                        
         BAS   RE,FLUSH                                                         
         LA    R4,DPTRECLN(R4)                                                  
         OC    0(DPTRECLN,R4),0(R4)                                             
         BNZ   ALLD5                                                            
*                                                                               
ALLDX    B     XIT                                                              
         EJECT                                                                  
***********************************                                             
* PRINTS SUBTOTA (NO GOALS) AND SUBTOTB (GOALS)                                 
* PRINT GRAND TOTAL                                                             
*                                                                               
FNLTOTS  NTR1                                                                   
         LA    R1,P                                                             
         USING PLINED,R1                                                        
         MVC   PDPT(10),=C'SUBTOTAL A'                                          
         LA    R2,SUBTOTA                                                       
         BAS   RE,FLUSH                                                         
         LA    RF,GRANDTOT                                                      
         BAS   RE,ROLLEM                                                        
         XC    SUBTOTA(28),SUBTOTA                                              
         BAS   RE,PRINTIT                SKIP LINE                              
         MVC   PDPT(10),=C'SUBTOTAL B'                                          
         LA    R2,SUBTOTB                                                       
         BAS   RE,FLUSH                                                         
         LA    RF,GRANDTOT                                                      
         BAS   RE,ROLLEM                                                        
         XC    SUBTOTB(28),SUBTOTB                                              
         BAS   RE,PRINTIT                 SKIP LINE                             
         MVC   PDPT(11),=C'GRAND TOTAL'                                         
         LA    R2,GRANDTOT                                                      
         BAS   RE,FLUSH                                                         
         XC    GRANDTOT(28),GRANDTOT                                            
         B     XIT                                                              
         EJECT                                                                  
************************************                                            
* PRINT LINE                                                                    
* INPUT R2-GOLDOLLARS OF IN AREA                                                
*                                                                               
FLUSH    NTR1                                                                   
         LA    R3,P1                                                            
         USING PLINED,R3                                                        
         EDIT  (B4,0(R2)),(9,PGOLDOL)                                           
         LA    R2,4(R2)                                                         
         EDIT  (B4,0(R2)),(9,PACTDOL)                                           
         MVC   ACTDOLSV,0(R2)                                                   
         LA    R2,4(R2)                                                         
         OC    0(4,R2),0(R2)                                                    
         BZ    FLU5                                                             
         EDIT  (B4,0(R2)),(7,PGOLGRP),1                                         
FLU5     MVC   GGRPSV,0(R2)                                                     
         LA    R2,4(R2)                                                         
         OC    0(4,R2),0(R2)                                                    
         BZ    FLU7                                                             
         EDIT  (B4,0(R2)),(7,PESTGRP),1                                         
FLU7     MVC   ESTGRPSV,0(R2)                                                   
         LA    R2,4(R2)                                                         
         OC    0(4,R2),0(R2)                                                    
         BZ    FLU9                                                             
         EDIT  (B4,0(R2)),(7,PACTGRP),1                                         
FLU9     MVC   ACTGRPSV,0(R2)                                                   
*                                                                               
         MVC   DUB(4),ACTGRPSV                                                  
         MVC   DUB+4(4),GGRPSV                                                  
         LA    RF,PGOLIND                                                       
         BAS   RE,EDIND                                                         
         MVC   DUB(4),ACTGRPSV                                                  
         MVC   DUB+4(4),ESTGRPSV                                                
         LA    RF,PESTIND                                                       
         BAS   RE,EDIND                                                         
*                                                                               
         MVC   DUB(4),ACTDOLSV                                                  
         MVC   DUB+4(4),4(R2)                                                   
         LA    RF,PCPMEST                                                       
         BAS   RE,EDCPM                                                         
         MVC   DUB(4),ACTDOLSV                                                  
         MVC   DUB+4(4),8(R2)                                                   
         LA    RF,PCPMACT                                                       
         BAS   RE,EDCPM                                                         
*                                                                               
         BAS   RE,PRINTIT                                                       
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
************************************                                            
* ADD DATA FROM ONE REC TO ANOTHER                                              
* R2-START OF INPUT DATA  RF-SATART OF OUTPUT DATA                              
*                                                                               
ROLLEM   NTR1                                                                   
         LA    R4,7                SEVEN FIELDS TO ROLL                         
ROLLOP   L     R1,0(R2)                                                         
         A     R1,0(RF)                                                         
         ST    R1,0(RF)                                                         
         LA    R2,4(R2)                                                         
         LA    RF,4(RF)                                                         
         BCT   R4,ROLLOP                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
****************************************                                        
* INPUT:  DIVIDEND IN DUB, DIVISOR IN DUB+4                                     
*         OUTAREA IN RF                                                         
*                                                                               
* OUTPUT: ROUNDED EDITED NUMBER IN OUTAREA, BINARY VALUE IN RF                  
*                                                                               
EDCPM    NTR1                                                                   
         OC    DUB(4),DUB                                                       
         BZ    EDCX                                                             
         OC    DUB+4(4),DUB+4                                                   
         BZ    EDCX                                                             
         LR    R2,RF                                                            
         L     RE,DUB                                                           
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         M     RE,=F'1000'                                                      
         D     RE,DUB+4                                                         
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         LTR   RF,RF                                                            
         BZ    EDCX                                                             
         EDIT  (RF),(5,0(R2)),2                                                 
         CLI   0(R2),X'40'                                                      
         BH    *+8                                                              
         MVI   0(R2),C'$'                                                       
EDCX     XIT1  REGS=(RF)                                                        
         EJECT                                                                  
****************************************                                        
* INPUT:  DIVIDEND IN DUB, DIVISOR IN DUB+4                                     
*         OUTAREA IN RF                                                         
* OUTPUT: +/- GRP INDEX IN OUTAREA, BINARY VALUE IN RF                          
*                                                                               
EDIND    NTR1                                                                   
         OC    DUB(4),DUB                                                       
         BZ    EDIX                                                             
         OC    DUB+4(4),DUB+4                                                   
         BZ    EDIX                                                             
         LR    R2,RF                                                            
         L     RE,DUB                                                           
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,DUB+4                                                         
         S     RF,=F'100'                                                       
         EDIT  (RF),(4,0(R2)),FLOAT=-                                           
         LTR   RF,RF                                                            
         BM    EDIX                                                             
         BNZ   EDI5                                                             
         MVC   2(2,R2),=C'+0'                                                   
         B     EDIX                                                             
EDI5     LA    R3,3(R2)                                                         
         LA    R4,4                                                             
PLUS     CLI   0(R3),C' '                                                       
         BE    EDI9                                                             
         BCTR  R3,0                                                             
         BCT   R4,PLUS                                                          
         B     *+8                                                              
EDI9     MVI   0(R3),C'+'                                                       
EDIX     XIT1  REGS=(RF)                                                        
         EJECT                                                                  
***************************************                                         
*                                                                               
GETDPTNM NTR1                                                                   
         USING SRTRECD,R3                                                       
         LA    R2,DPTTBL                                                        
GD3      CLC   SDPT,0(R2)                                                       
         BE    GD5                                                              
         LA    R2,9(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   GD3                                                              
         MVC   DPTNAME,=C'UNKNOWN '                                             
         B     *+10                                                             
GD5      MVC   DPTNAME,1(R2)                                                    
         MVC   CURDPT,SDPT                                                      
GDX      B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
*        DAYPART NAME TABLE                                                     
DPTTBL   DC    CL1'D',CL8'DAYTIME'                                              
         DC    CL1'F',CL8'FRINGE'                                               
         DC    CL1'P',CL8'PRIME'                                                
         DC    CL1'K',CL8'KIDS'                                                 
         DC    CL1'S',CL8'SPORTS'                                               
         DC    CL1'N',CL8'NEWS'                                                 
         DC    CL1'L',CL8'LATE'                                                 
         DC    CL1'Y',CL8'YOUTH'                                                
         DC    CL1'E',CL8'EARLY'                                                
         DC    CL1'T',CL8'TEENS'                                                
         DC    CL1'C',CL8'CABLE'                                                
         DC    CL1'X',CL8'SYND'                                                 
         DC    CL1'I',CL8'SPECIAL'                                              
         DC    XL1'FF',CL8' '      END OF TABLE                                 
         EJECT                                                                  
*                                                                               
********************************                                                
GETPRDNM NTR1                                                                   
*        EXPECTS 3 CHAR PRD CODE IN FULL                                        
*                                                                               
         USING WRKRECD,R2                                                       
         CLI   FULL,0                                                           
         BNE   GET5                                                             
         MVC   PRODNAME,=C'UNKNOWN'                                             
         XC    CURPRD,CURPRD                                                    
         B     GPX                                                              
GET5     XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(3),FULL           3 CHAR PRD CODE                          
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         USING PRDHDR,R4                                                        
         MVC   PRODNAME,PNAME                                                   
         MVC   CURPRD,FULL                                                      
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
GPX      DS    0H                                                               
         B     XIT                                                              
         DROP  R2                                                               
         SPACE                                                                  
**********************************                                              
*  GET 3CHAR CODE FROM C LIST                                                   
*                                                                               
PRDCD    NTR1                                                                   
         L     R3,ACLIST                                                        
PCD3     CLC   3(1,R3),NBSPLPRN                                                 
         BE    PCD5                                                             
         LA    R3,4(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   PCD3                                                             
         MVC   WORK(3),=3X'00'                                                  
         B     *+10                                                             
PCD5     MVC   WORK(3),0(R3)                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************                                              
*                                                                               
ROUT1    NTR1                                                                   
         BAS   RE,GETPRDNM                                                      
         BAS   RE,GETDPTNM                                                      
         BAS   RE,WRTDPT                                                        
         B     XIT                                                              
         SPACE                                                                  
****************************                                                    
*                                                                               
WRTDPT   NTR1                                                                   
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         MVC   PDPT(7),DPTNAME                                                  
         MVC   PDPT+8(6),=C'(CONT)'                                             
         MVC   H1SAVE,P                                                         
         XC    PDPT+8(6),PDPT+8                                                 
         LA    R3,PDPT                                                          
         LA    R2,132(R3)                                                       
WDP5     MVI   0(R2),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R3),X'40'                                                      
         BH    WDP5                                                             
         MVC   H2SAVE,P2                                                        
         DS    0H                                                               
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************                                             
* GET IMPRESSION FOR GOALS -(UNIV X GRP = IMPS (ROUNDED))                       
* INPUT: GOALGRP IN FULL                                                        
GIMPS    NTR1                                                                   
         XC    GOALIMP,GOALIMP                                                  
         OC    FULL,FULL           NO GOAL GRP/SKIP                             
         BZ    GIX                                                              
         MVC   HALF,FULL+2                                                      
         L     R0,UNIV                                                          
         MH    R0,HALF             GOAL GRP                                     
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'1000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,GOALIMP                                                       
GIX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+17(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         CLC   =C'POL',SPLPRO                                                   
         BE    *+16                                                             
         MVC   H4+10(3),CURPRD                                                  
         MVC   H4+17(20),PRODNAME                                               
         CLI   SPLPRO+1,C'='                                                    
         BNE   HD5                                                              
         CLC   SPLPRO+2(3),=C'ALL'                                              
         BNE   *+8                                                              
         MVI   PGRALL,C'Y'                                                      
         XC    H4(30),H4                                                        
*        MVC   H4+10(5),NBSELPGR                                                
*        XC    H4+17(20),H4+17                                                  
         BAS   RE,PRDGRPHD                                                      
HD5      DS    0H                                                               
         LA    R2,H5                                                            
         OC    0(10,R2),0(R2)                                                   
         BZ    *+8                                                              
         LA    R2,132(R2)                                                       
         MVC   0(8,R2),=C'ESTIMATE'                                             
         MVC   10(6,R2),SPLEST                                                  
         MVC   17(20,R2),SPLESTN                                                
         DROP  R5                                                               
         SPACE                                                                  
         LA    R2,H13                                                           
         USING PLINED,R2                                                        
         MVC   PGOLDOL+8(7),=C'DOLLARS'                                         
         MVC   PGOLDOL+137(4),=C'GOAL'                                          
         MVC   PACTDOL+138(3),=C'ACT'                                           
         MVC   PGOLGRP+134(4),=C'GOAL'                                          
         MVC   PESTGRP+135(3),=C'EST'                                           
         MVC   PACTGRP+135(3),=C'ACT'                                           
         MVC   PACTGRP+15(3),=C'IND'                                            
         MVC   PGOLIND+133(3),=C'GOL'                                           
         MVC   PESTIND+133(3),=C'EST'                                           
         MVC   PCPMEST+5(3),=C'CPM'                                             
         LA    R2,PCPMEST+132                                                   
         MVC   2(3,R2),=C'EST'                                                  
         MVC   9(3,R2),=C'ACT'                                                  
*                                                                               
DEMNAME  LA    R2,H13              PRINT OUT DEMO NAME                          
         LA    R4,DBLOCK                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBCOMFCS,ACOMFACS   *SET FOR DEMOCON                             
         MVC   DBFILE,=C'NTI'      *                                            
         MVI   DBSELMED,C'N'       *                                            
         LA    R3,NDDEMOS                                                       
         L     R1,ANETWS1        ****                                           
         MVC   0(10,R1),0(R3)    ****                                           
         LA    R5,PGOLGRP+8                                                     
         GOTO1 ADEMOCON,DMCB,(1,(R3)),(2,(R5)),(0,(R4))                         
         CLI   SKIPHD,C'Y'                                                      
         BE    BOXES                                                            
         CLC   P(21),H1SAVE                                                     
         BE    BOXES                                                            
         MVC   P3,P                                                             
         MVC   P,H1SAVE                                                         
         MVC   P2,H2SAVE                                                        
         SPACE                                                                  
BOXES    DS    0H                   SET PARAMS FOR BOXES                        
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         DROP  R2                                                               
         LA    R5,BOXCOLS                                                       
         USING PLINED,R5                                                        
         MVI   PDPT-1,C'L'                                                      
         MVI   PGOLDOL-1,C'C'                                                   
         MVI   PGOLGRP-1,C'C'                                                   
         MVI   PGOLIND-2,C'C'                                                   
         MVI   PCPMEST-2,C'C'                                                   
         MVI   PCPMACT+7,C'R'                                                   
BOX7     LA    R5,BOXROWS                                                       
         LA    R5,11(R5)                                                        
         MVI   0(R5),C'T'                                                       
         LA    R5,3(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
PRDGRPHD NTR1                                                                   
         L     R2,ACLIST           GET 1 BYTE PRD CODE                          
PRDLOOP  CLC   0(3,R2),CURPRD                                                   
         BE    PRDL3                                                            
         LA    R2,4(R2)                                                         
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         B     PRDLOOP                                                          
PRDL3    ZIC   R1,3(R2)            R1 NOW HAS 1 BYTE PRD CODE                   
*                                                                               
         LA    R2,NDPRGBUF         POSITIONAL 2X220 PRD GRP CODES               
         BCTR  R1,0                POSITIONAL PRD NO                            
         CH    R1,=H'0'                                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
         SLA   R1,1                                                             
         AR    R2,R1                                                            
         MVC   WORK+1(2),0(R2)     2 BYTE CODE                                  
         MVC   WORK(1),NBSELPGR    SCHEME CODE                                  
         LA    R2,WORK                                                          
         BAS   RE,PGRPAGE          FILLS IN PRDGRP NAMES IN DBLOCK              
*                                                                               
         LA    R2,H4               R2=A(DESCRIPTION)                            
         LA    R3,H4+10            R3=A(CODE)                                   
         LA    R4,H4+18            R4=A(NAME)                                   
         CH    R1,=H'9'            IS PRDGRP NAME FORCING A SHIFT               
         BNH   PRDHD5                                                           
         AH    R3,=H'4'            YES/ADD TO REGS                              
         AH    R4,=H'4'                                                         
PRDHD5   LA    R5,NDPRGBK1+11                                                   
         LA    R1,12                                                            
LOOP2    CLI   0(R5),X'40'                                                      
         BH    XLOOP2                                                           
         BCTR  R5,0                                                             
         BCT   R1,LOOP2                                                         
         LTR   R1,R1                                                            
         BNZ   XLOOP2                                                           
         DC    H'0'                                                             
XLOOP2   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NDPRGBK1                                                 
         MVC   0(4,R3),NDPRGAB1                                                 
         MVC   0(24,R4),NDPRGNM1                                                
         OC    NDPRGAB2,NDPRGAB2   IS IT A TWO LEVEL BREAK                      
         BZ    PRDHD7                                                           
         LA    R2,132(R2)                                                       
         LA    R3,132(R3)                                                       
         LA    R4,132(R4)                                                       
         LA    R5,NDPRGBK2+11                                                   
         LA    R1,12                                                            
LOOP3    CLI   0(R5),X'40'                                                      
         BH    XLOOP3                                                           
         BCTR  R5,0                                                             
         BCT   R1,LOOP3                                                         
         LTR   R1,R1                                                            
         BNZ   XLOOP3                                                           
         DC    H'0'                                                             
XLOOP3   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NDPRGBK2                                                 
         MVC   0(4,R3),NDPRGAB2                                                 
         MVC   0(24,R4),NDPRGNM2                                                
PRDHD7   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL IN PRODUCT GROUP DETAILS                         
         SPACE 3                                                                
*              INPUT               R2 = A(3-BYTE PRODUCT GROUP CODE)            
*                                *****  CARRIED IN WORK  ***                    
         SPACE 1                                                                
PGRPAGE  NTR1                                                                   
         CLC   PRGRPSV,0(R2)      HAVE WE DONE THIS ONE BEFORE                  
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   PRGRPSV,0(R2)      3-BYTE CODE                                   
*                                  NEED FAIR BIT HERE                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,NDAGYKEY                                                
         MVC   PRGKCLT,CLTSV                                                    
         MVC   PRGKID,0(R2)              GET PROG GRP DEF REC                   
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRGEL01,R4                                                       
         MVI   NDPRGNLV,1                                                       
         MVC   NDPRGBK1,PRGBK1                                                  
         CLI   PRGBK2,0                                                         
         BE    PGC5                                                             
         MVI   NDPRGNLV,2                                                       
         MVC   NDPRGBK2,PRGBK2                                                  
PGC5     MVC   LEVELN(1),PRGBK1LN           SAVE DIGIT LENGTH OF LEVELS         
         MVC   LEVELN+1(1),PRGBK2LN                                             
*                                                                               
         CLI   PGRALL,C'Y'         IF N=ALL/END HERE                            
         BE    XIT                                                              
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         MVC   PRGKID(3),0(R2)               GET SPECIFIC PROG GRP REC          
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         XC    FULL,FULL                                                        
         MVC   NDPRGAB1(1),PRGKID                                               
         MVC   FULL+1(2),PRGKGRP                                                
         MVI   FULL+3,X'0C'          ADD X'0C' SINCE PRGKGRP IS PWOS            
         UNPK  WORK(5),FULL+1(3)                                                
         LA    R5,NDPRGAB1+1                                                    
         ZIC   R1,LEVELN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK      ** NOTE DO NOT TOUCH WORK NEEDED BELOW         
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRGEL10,R4                                                       
         MVC   NDPRGNM1,PRGNAM1                                                 
         ZIC   R1,LEVELN           IS THERE A SECOND LEVEL                      
         ZIC   R5,LEVELN+1                                                      
         LTR   R5,R5                                                            
         BZ    PGCX                                                             
         AR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NDPRGAB2+1(0),WORK      SET CODE(** NOTE WORK HAS CODE)          
         MVC   NDPRGAB2(1),NDPRGAB1    SET SCHEME LETTER                        
         MVC   NDPRGNM2,PRGNAM2                                                 
*                                                                               
PGCX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'PRODUCT POST ANALYSIS'                                   
         SSPEC H2,52,C'---------------------'                                   
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         SPACE                                                                  
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
WORKAREA DS    CL50000                                                          
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
ACLIST   DS    A                                                                
UNIV     DS    F                                                                
ANETGOAL DS    A                                                                
ADEMOCON DS    A                                                                
GOALIMP  DS    F                                                                
ABOXCOLS DS    F                                                                
AH10     DS    F                                                                
AP1      DS    F                                                                
APOUT    DS    F                                                                
NUMPER   DS    F                                                                
ACTDOLSV DS    F                                                                
GGRPSV   DS    F                                                                
ESTGRPSV DS    F                                                                
ACTGRPSV DS    F                                                                
ACUR1    DS    F                                                                
FRST     DS    CL1                                                              
PEROPT   DS    CL1                                                              
PREVREC  DS    CL1                                                              
CURDPT   DS    CL1                                                              
CURMED   DS    CL1                                                              
CURPRD   DS    CL3                                                              
CUREST   DS    CL1                                                              
PERTYPE  DS    CL4                                                              
PRDCDSV  DS    CL3                                                              
DPTNAME  DS    CL8                                                              
MYWORK   DS    CL150                                                            
PRODNAME DS    CL20                                                             
*                                                                               
PRGRPSV  DS    CL3                                                              
CLTSV    DS    CL2                                                              
PGRALL   DS    CL1                                                              
LEVELN   DS    CL2                                                              
BOXSET   DS    CL1                                                              
PRODCODE DS    CL3                                                              
SKIPHD   DS    CL1                                                              
H1SAVE   DS    CL132                                                            
H2SAVE   DS    CL132                                                            
*                                                                               
*                                                                               
DPTOTAL  DS    0F                  AREA FOR DAYPART TOTALS                      
DPTPRDNM DS    CL20                                                             
DPTGDOL  DS    F                   GOAL DOLLARS                                 
DPTACT   DS    F                   ACTUAL DOLLARS                               
DPTGGRP  DS    F                   GOAL GRP                                     
DPTDEME  DS    F                   EST DEM                                      
DPTDEMA  DS    F                   ACT DEM                                      
DPTIMPE  DS    F                   EST IMP                                      
DPTIMPA  DS    F                   ACT IMP                                      
DPTRECLN EQU   *-DPTOTAL                                                        
*                                                                               
SUBTOTA  DS    0F                                                               
         DS    7F                                                               
*                                                                               
SUBTOTB  DS    0F                                                               
         DS    7F                                                               
*                                                                               
GRANDTOT DS    0F                                                               
         DS    7F                                                               
*                                                                               
MYDLENE  EQU   *-MYD                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
WRKRECD  DSECT                                                                  
WDPT     DS    CL1                 DAYPART                                      
WPRD     DS    CL3                 PRODUCT CODE                                 
WKEYLENE EQU   *-WDPT                                                           
WTYPE    DS    CL1                 TYPE(1=GOAL,2=UNIT)                          
WGOAL    DS    CL1                 TARGET DEMO                                  
WGDOL    DS    F                   GOAL DOLLARS                                 
WACTDOL  DS    F                   ACTUAL DOLLARS                               
WGGRP    DS    F                   GOAL GRP                                     
W1DEME   DS    F                   EST DEM                                      
W1DEMA   DS    F                   ACT DEM                                      
W1IMPE   DS    F                   EST IMP                                      
W1IMPA   DS    F                   ACT IMP                                      
WDEMLENE EQU   *-W1DEME                                                         
WRECLENE EQU   *-WDPT                                                           
*                                                                               
*                                                                               
SRTRECD  DSECT                                                                  
SDPT     DS    CL1                 DAYPART                                      
SPRD     DS    CL3                 PRODUCT CODE                                 
SKEYLENE EQU   *-SDPT                                                           
STYPE    DS    CL1                 TYPE(1=GOAL,2=UNIT)                          
SGOAL    DS    CL1                 TARGET DEMO                                  
SGDOL    DS    F                   GOAL DOLLARS                                 
SACTDOL  DS    F                   ACTUAL DOLLARS                               
SGGRP    DS    F                   GOAL GRP                                     
S1DEME   DS    F                   EST DEM                                      
S1DEMA   DS    F                   ACT DEM                                      
S1IMPE   DS    F                   EST IMP                                      
S1IMPA   DS    F                   ACT IMP                                      
SDEMLENE EQU   *-S1DEME                                                         
SRECLENE EQU   *-SDPT                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
PRDTOTD  CSECT              TOTALS FOR RECAP BY PRODUCT                         
PRDPROD  DS    CL3                 PRODUCT                                      
PRDGDOL  DS    F                   GOAL DOLLARS                                 
PRDACT   DS    F                   ACTUAL DOLLARS                               
PRDGGRP  DS    F                   GOAL GRP                                     
PRDDEME  DS    F                   EST DEM                                      
PRDDEMA  DS    F                   ACT DEM                                      
PRDIMPE  DS    F                   EST IMP                                      
PRDIMPA  DS    F                   ACT IMP                                      
PRDRECLN EQU   *-PRDPROD                                                        
         EJECT                                                                  
*                                                                               
PLINED   DSECT                    PRINT LINE DSECT                              
         DS    CL1                                                              
         DS    CL13                                                             
PDPT     DS    CL20                                                             
         DS    CL5                                                              
PGOLDOL  DS    CL9                                                              
         DS    CL1                                                              
PACTDOL  DS    CL9                                                              
         DS    CL3                                                              
PGOLGRP  DS    CL7                                                              
         DS    CL2                                                              
PESTGRP  DS    CL7                                                              
         DS    CL2                                                              
PACTGRP  DS    CL7                                                              
         DS    CL5                                                              
PGOLIND  DS    CL5                                                              
         DS    CL1                                                              
PESTIND  DS    CL5                                                              
         DS    CL3                                                              
PCPMEST  DS    CL5                                                              
         DS    CL2                                                              
PCPMACT  DS    CL5                                                              
PLINLENE EQU   *-PDPT                                                           
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE3D                                                       
*****  ++INCLUDE DRDICFILE                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE SPGENPRG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086NEWRI25   05/01/02'                                      
         END                                                                    
