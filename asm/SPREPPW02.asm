*          DATA SET SPREPPW02  AT LEVEL 065 AS OF 05/01/02                      
*PHASE SPPW02A                                                                  
         TITLE 'SPREPPW02 - MEDIA PLANNING AGENCY SPOT EXTRACT'                 
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
*        QOPT1   A=ADI,N=DMA                                                    
*        QOPT2   DPG PHASE TO USE - (A THRU D = DOWNLOAD)                       
*        QOPT3   C=USE CLIENT DATE, A=AGY, I=INDUSTRY                           
*        QOPT4   QUARTER NUMBER                                                 
*        QOPT5   T=TRACE CPP RECORDS                                            
*        QOPT5+1 DRIVER TEST PHASE                                              
*                                                                               
*   NOTE- THERE IS SOME VESTIGIAL STRUCTURE IN THIS PROGRAM TO HANDLE           
*         MULTIPLE QUARTERS, BUT IT IS NOT REALLY OPERATIONAL.                  
*         IT ONLY DOES ONE QUARTER PER FILE.                                    
*                                                                               
SPPW02   CSECT                                                                  
         NMOD1 0,SPPW02,RR=R5                                                   
*                                                                               
         LA    R8,2048(RB)         R8 IS 2ND BASE REG                           
         LA    R8,2048(R8)                                                      
         USING SPPW02+4096,R8                                                   
         L     R9,0(R1)                                                         
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,R9,RA                                                    
         ST    R5,RELO                                                          
*                                                                               
         L     RC,=A(GLAREA)                                                    
         A     RC,RELO                                                          
         ST    RC,VGLAREA                                                       
         USING GLOBALD,RC                                                       
         L     RF,=A(MKTTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,AMKTTAB                                                       
         L     RF,=A(USTTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,AUSTTAB                                                       
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         B     EXIT                                                             
         SPACE 2                                                                
* RUN FIRST                                                                     
*                                                                               
RUNF     DS    0H                                                               
         XC    ADRIVER,ADRIVER                                                  
         XC    ASYSDRV,ASYSDRV                                                  
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
* REQUEST FIRST                                                                 
*                                                                               
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   DLHSW,0                                                          
*                                                                               
         LA    RE,GLOBALD          INIT DRIVER STORAGE TO ZEROS                 
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         MVC   GLSIZE,=F'40000'    GLOBAL SIZE                                  
         MVC   GLOBALD(8),=C'*GLOBAL*'                                          
*                                                                               
         MVI   GLTRACE,C'N'                                                     
         NI    GLDOWNLD,X'3F'                                                   
         CLI   QOPT2,C'N'          TEST DOWNLOADING                             
         BNE   *+8                                                              
         MVI   QOPT2,C' '          'N' TO BLANK                                 
         MVC   DPGFILE+6(1),QOPT2                                               
         CLI   QOPT2,C' '          TEST DOWNLOADING                             
         BE    SP109                                                            
         CLI   QOPT2,C'D'          A THRU D = DOWNLOADING                       
         BH    SP109                                                            
         OI    GLDOWNLD,X'C0'                                                   
*                                                                               
SP109    DS    0H                                                               
         GOTO1 LOADER,DMCB,DPGFILE,0    LOAD DPG PROGRAM                        
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,GLAPROG                                                       
*                                                                               
         OC    ADRIVER,ADRIVER                                                  
         BNZ   SP110                                                            
         MVC   DRIVER+6(1),QOPT5+1      SET TEST DRIVER VERSION **TEST          
         GOTO1 LOADER,DMCB,DRIVER       LOAD DRIVER                             
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ADRIVER                                                       
*                                                                               
SP110    OC    ASYSDRV,ASYSDRV                                                  
         BNZ   SP120                                                            
         GOTO1 LOADER,DMCB,SPDRIVER     LOAD SYSTEM DRIVER                      
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ASYSDRV                                                       
*                                                                               
SP120    MVC   GLASYSDR,ASYSDRV    SYSTEM DRIVER ADDR                           
*                                                                               
         LA    RE,SPWORKD                                                       
         ST    RE,GLAWORKD         SPOT WORK ADDR                               
         LA    RE,DRHOOK                                                        
         ST    RE,GLAHOOK          OUR DRIVER HOOK                              
         MVI   GLTWORKD,GLTSPOT    SPOT WORK                                    
         MVI   GLDETHED,C'Y'       I GET HEADS AT DETAIL TIME                   
         LA    RE,PROGPROF                                                      
         ST    RE,GLAPPROF                                                      
         L     RE,GLAPLIST                                                      
         MVC   ASPDRWKC,0(RE)      A(SPOT DRIVER WORK AREA)                     
*                                                                               
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
*                                                                               
REQF50   DS    0H                                                               
         BAS   RE,GETCPP           GET CPP DATA AND PASS TO DRIVER              
*                                                                               
         MVI   GLMODE,GLOUTPUT     DRIVER OUTPUT PHASE                          
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
DRHOOK   NTR1                                                                   
*                                                                               
*        DRIVER HOOK ROUTINE                                                    
*                                                                               
         USING MKTTABD,R6           R6 WILL POINT TO MKT ENTRY                  
*                                                                               
         CLI   GLHOOK,GLROUT       TEST TO EXECUTE USER ROUTINE                 
         BNE   DH005                                                            
         CLI   GLMODE,GLINPUT      YES - INPUT PHASE                            
         BE    DH100                                                            
         CLI   GLMODE,GLOUTPUT           OUTPUT PHASE                           
         BE    DH200                                                            
         DC    H'0'                                                             
*                                                                               
DH005    CLI   GLHOOK,GLRESOLV     TEST TO RESOLVE LABELS                       
         BE    DH010                                                            
         CLI   GLHOOK,GLRESLIT     TEST TO RESOLVE LITERAL                      
         BE    DH300                                                            
         CLI   GLHOOK,GLHEAD       HEADLINES                                    
         BE    DH400                                                            
         CLI   GLHOOK,GLPRINT      PRINT                                        
         BE    DH500                                                            
         CLI   GLHOOK,GLPUTSRT     PUT TO SORT OR NOT                           
         BE    DH450                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* RESOLVE ROUTINE LABELS                                                        
*                                                                               
DH010    LA    R1,ROUTLIST                                                      
         LA    R2,GLLABEL                                                       
*                                                                               
DH020    CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         CLC   0(8,R1),0(R2)                                                    
         BE    DH030                                                            
         LA    R1,12(R1)                                                        
         B     DH020                                                            
*                                                                               
DH030    MVC   GLAROUT,8(R1)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
* EXECUTE INPUT PHASE ROUTINES                                                  
*                                                                               
DH100    DS    0H                                                               
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BZ    DH190                                                            
         CLI   GLRECNO,1           FOR RECORD 1                                 
         BNE   DH104                                                            
         CLI   DLHSW,0             TEST HAVE DONE HEADS                         
         BNE   EXIT                NOTE - SWITCH IS SET AT DH450                
*                                                                               
DH104    DS    0H                                                               
DH190    DS    0H                                                               
         L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
* EXECUTE OUTPUT PHASE ROUTINES                                                 
*                                                                               
DH200    L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
* INPUT ROUTINES                                                                
*                                                                               
MKTNUM   DS    0H                  MARKET                                       
         MVC   0(2,R3),MKNUM       RATING SERVICE MKT                           
         B     EXIT                                                             
*                                                                               
MKTNAM   DS    0H                  MKT NAME                                     
         MVC   0(26,R3),MKNAM                                                   
         B     EXIT                                                             
*                                                                               
MKTPOP   DS    0H                  POPULATIONS                                  
         ZIC   RF,GLARGS           ARG IS DEMO                                  
         SLL   RF,3                                                             
         LA    R1,MKUNVS-8(RF)                                                  
         MVC   0(8,R3),0(R1)                                                    
         B     EXIT                                                             
*                                                                               
STATIP   DS    0H                                                               
         MVC   0(5,R3),STA                                                      
         CLI   DRPASS,1                                                         
         BE    *+10                                                             
         MVC   0(5,R3),=C'*ALL*'                                                
         B     EXIT                                                             
*                                                                               
STUBIP   DS    0H                  STUB                                         
         CLI   GLARGS,1            1 = SPOTS                                    
         BNE   *+14                                                             
         MVC   0(5,R3),=C'SPOTS'                                                
         B     EXIT                                                             
         CLI   GLARGS,2            2 = COST                                     
         BNE   *+14                                                             
         MVC   0(4,R3),=C'COST'                                                 
         B     EXIT                                                             
         CLI   GLARGS,3            3 = IMPS                                     
         BNE   *+14                                                             
         MVC   0(7,R3),=C'HH IMPS'                                              
         B     EXIT                                                             
         CLI   GLARGS,11           11 = SPACER                                  
         BNE   *+12                                                             
         MVI   0(R3),X'FF'         FORCE PRINT OF LINE                          
         B     EXIT                                                             
         ZIC   RF,GLARGS                                                        
         SH    RF,=H'4'            ELSE ARG-3 IS DEMO                           
         MH    RF,=H'12'                                                        
         LA    RF,DNAMES(RF)                                                    
         MVC   0(5,R3),7(RF)                                                    
         B     EXIT                                                             
*                                                                               
DLHQTR   DS    0H                  QUARTER-BASED ON REQ START MONTH             
         MVI   0(R3),1                                                          
         CLC   CQSTAM+2(2),=C'03'                                               
         BNH   EXIT                                                             
         MVI   0(R3),2                                                          
         CLC   CQSTAM+2(2),=C'06'                                               
         BNH   EXIT                                                             
         MVI   0(R3),3                                                          
         CLC   CQSTAM+2(2),=C'09'                                               
         BNH   EXIT                                                             
         MVI   0(R3),4                                                          
         B     EXIT                                                             
*                                                                               
SPTSIP   DS    0H                  SPOTS                                        
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         USING MKDPD,R4                                                         
         CP    MKDPSPT,=P'0'                                                    
         BE    *+8                                                              
         MVI   ACTRECSW,1          SET ACTIVE                                   
         MVC   0(8,R3),MKDPSPT                                                  
         B     EXIT                                                             
*                                                                               
COSTIP   DS    0H                  COST                                         
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         CP    MKDPCST,=P'0'                                                    
         BE    *+8                                                              
         MVI   ACTRECSW,1          SET ACTIVE                                   
         MVC   0(8,R3),MKDPCST                                                  
         B     EXIT                                                             
*                                                                               
IMPSIP   DS    0H                  IMPS                                         
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         CP    MKDPIMP,=P'0'                                                    
         BE    *+8                                                              
         MVI   ACTRECSW,1          SET ACTIVE                                   
         MVC   0(8,R3),MKDPIMP                                                  
         B     EXIT                                                             
*                                                                               
GRPSIP   DS    0H                  GRPS                                         
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         ZIC   RF,GLARGS+1         2ND ARG IS DEMO                              
         SLL   RF,3                X 8                                          
         LA    R1,MKDPGRPS-8(RF)                                                
         MVC   0(8,R3),0(R1)                                                    
         CP    0(8,R1),=P'0'                                                    
         BE    *+8                                                              
         MVI   ACTRECSW,1          SET ACTIVE                                   
         B     EXIT                                                             
*                                                                               
DCFIN    DS    0H                  DCF'S                                        
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         ZIC   RF,DEMNO                                                         
         SLL   RF,3                                                             
         LA    R1,MKDPGRPS(RF)                                                  
         CVB   R1,0(R1)                                                         
         M     R0,=F'10000'                                                     
         CVB   RF,MKDPGRPS         HH GRPS                                      
         BAS   RE,DIV                                                           
         CVD   R1,0(R3)                                                         
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         MVI   ACTRECSW,1          SET ACTIVE                                   
         B     EXIT                                                             
*                                                                               
MDPQDSP  DS    0H                  GET DISP FOR DPT/QTR                         
         ZIC   R4,GLARGS           ARG IS DPT NUM                               
         BCTR  R4,R0                                                            
         MH    R4,=Y(NQTRS)        X MAX QTRS                                   
         ZIC   R1,QTRNO                                                         
         AR    R4,R1               PLUS QTRNO                                   
         MH    R4,=Y(MKDPDL)       X ENTRY LENGTH                               
         LA    R4,MKDPQW(R4)       POINT TO RIGHT SLOT                          
         BR    RE                                                               
         DROP  R4                                                               
*                                                                               
DEMNUM   DS    0H                  DEMO NUMBER                                  
         MVC   0(1,R3),DEMNO                                                    
         B     EXIT                                                             
*                                                                               
DEMNAM   DS    0H                  DEMO NAME                                    
         ZIC   RF,DEMNO            NUMBER IS INDEX                              
         MH    RF,=H'12'                                                        
         LA    RF,DNAMES(RF)                                                    
         MVC   0(5,R3),7(RF)       LAST 5 POSITIONS HAVE DEMO                   
         B     EXIT                                                             
*                                                                               
*                                                                               
DLHTTL   DS    0H                  DOWNLOAD HEADER                              
         MVC   0(6,R3),=C'CPPCMB'  COMBINED                                     
         CLI   QOPT3,C'I'                                                       
         BE    DLHTTL2                                                          
         MVC   3(3,R3),=C'AGY'     AGENCY                                       
         CLI   QOPT3,C'A'                                                       
         BE    DLHTTL2                                                          
         MVC   3(3,R3),=C'CLT'     CLIENT                                       
*                                                                               
DLHTTL2  DS    0H                                                               
         MVI   ACTRECSW,1          HEADER REC ALWAYS ACTIVE                     
         B     EXIT                                                             
*                                                                               
DLHSRC   DS    0H                                                               
         MVC   0(4,R3),=C'ARB '    SOURCE                                       
         CLI   QOPT1,C'A'                                                       
         BE    *+10                                                             
         MVC   0(4,R3),=C'ACN '                                                 
         B     EXIT                                                             
*                                                                               
DLHRUND  DS    0H                  RUN DATE                                     
         MVC   0(6,R3),TODAY                                                    
         B     EXIT                                                             
*                                                                               
DLHRQID  DS    0H                  REQUESTING ID                                
         L     RF,LOGOC                                                         
         USING LOGOD,RF                                                         
         MVC   0(4,R3),LOGOJOB                                                  
         CLI   LOGOJOB,C' '                                                     
         BH    *+10                                                             
         MVC   0(4,R3),=C'RQID'                                                 
         DROP  RF                                                               
         B     EXIT                                                             
*                                                                               
DLHADV   DS    0H                  ADVERTISER                                   
         MVC   0(3,R3),CQCLT                                                    
         B     EXIT                                                             
*                                                                               
DLHPRD   DS    0H                  PRODUCT                                      
         MVC   0(3,R3),=C'ALL'                                                  
         B     EXIT                                                             
*                                                                               
DLHEST   DS    0H                  ESTIMATE                                     
         MVC   0(7,R3),TRGDEM      =TARGET DEMO                                 
         B     EXIT                                                             
*                                                                               
DLHSTD   DS    0H                  START MONTH                                  
         MVC   0(4,R3),CQSTAM                                                   
         B     EXIT                                                             
*                                                                               
DLHEND   DS    0H                  END MONTH                                    
         MVC   0(4,R3),CQENDM                                                   
         B     EXIT                                                             
*                                                                               
DLHNDEM  DS    0H                  NUMBER OF DEMOS (EXCLUDING HH)               
         L     R1,NADEMS                                                        
         STC   R1,0(R3)                                                         
         B     EXIT                                                             
*                                                                               
DLHNMKT  DS    0H                  NUMBER OF MARKETS                            
         XC    0(2,R3),0(R3)       NOT NEEDED DNOW                              
         B     EXIT                                                             
*                                                                               
DLHNDPT  DS    0H                                                               
         MVI   0(R3),NDPTS                                                      
         B     EXIT                                                             
*                                                                               
DLHDEML  DS    0H                  LIST OF DEMO NAMES                           
         LR    RE,R3                                                            
         ICM   R0,15,NADEMS                                                     
         BZ    EXIT                NONE BUT HH                                  
         LA    RF,DNAMES+12                                                     
*                                                                               
DLHDL2   DS    0H                                                               
         MVC   0(5,RE),7(RF)                                                    
         LA    RE,5(RE)                                                         
         LA    RF,12(RF)                                                        
         BCT   R0,DLHDL2                                                        
         B     EXIT                                                             
*                                                                               
DLHDPTL  DS    0H                                                               
         MVC   0(NDPTS*2,R3),DPTLST                                             
         B     EXIT                                                             
*                                                                               
* OUTPUT ROUTINES                                                               
*                                                                               
DPTNAM   DS    0H                                                               
         ZIC   RF,GLARGS           DPT NUMBER                                   
         SLL   RF,1                                                             
         LA    RF,DPTLST-2(RF)                                                  
         MVC   0(2,R3),0(RF)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
DLHNMKTO DS    0H                  NUMBER OF ACTIVE MARKETS                     
         MVI   GLHOOK,GLEDIT                                                    
         MVC   0(2,R2),AMKTCNT+2                                                
         B     EXIT                                                             
         SPACE 2                                                                
* RESOLVE LITERALS                                                              
*                                                                               
DH300    DS    0H                                                               
         DC    H'0'                                                             
*                                                                               
         SPACE 2                                                                
* HEADLINES                                                                     
*                                                                               
DH400    DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        TEST TO PUT TO SORT OR NOT                                             
*                                                                               
DH450    DS    0H                                                               
         CLI   ACTRECSW,1          SKIP INACTIVE RECS                           
         BC    0,DH499X       ***  NO-OP ACTIVE RECORD TEST ***                 
         MVI   ACTRECSW,0          RESET                                        
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BZ    DH460                                                            
         CLI   GLRECNO,1           FOR RECORD 1 (HEADER)                        
         BNE   DH452                                                            
         CLI   DLHSW,0             TEST HAVE DONE HEADS                         
         BNE   DH499X              YES - DONT DO AGAIN                          
         MVI   DLHSW,1                                                          
         B     EXIT                                                             
*                                                                               
DH452    DS    0H                                                               
         CLI   GLRECNO,2           FOR RECORD 2                                 
         BNE   DH454                                                            
         CLI   DRPASS,C'D'         MUST BE DCF PASS                             
         BE    EXIT                                                             
         B     DH499X                                                           
*                                                                               
DH454    DS    0H                                                               
         CLI   DRPASS,C'D'         SKIP OTHER RECS FOR DCF PASS                 
         BE    DH499X                                                           
         B     EXIT                ***NO-OP QUARTERLY TEST**                    
         CLI   GLRECNO,4           FOR REC 4 (SPOTS)                            
         BE    EXIT                DO FOR ALL QTRS                              
         CLI   QTRNO,0             ELSE DO ONLY FOR TOTAL                       
         BE    EXIT                                                             
         B     DH499X                                                           
*                                                                               
DH460    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
DH499X   DS    0H                                                               
         MVI   GLHOOK,GLDONT                                                    
         B     EXIT                                                             
         SPACE 2                                                                
*        PRINT                                                                  
*                                                                               
DH500    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        GETCPP - GET CPP DATA                                                  
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
GETCPP   NTR1                                                                   
         BAS   RE,BLDMTAB          BUILD MARKET LIST OF DMAS                    
*                                                                               
         OPEN  (CPPFIL,(INPUT))                                                 
         L     RE,AUSTTAB          CLEAR US TOTAL AREA                          
         LA    RF,MKLEN                                                         
         XCEF                                                                   
         L     RE,AUSTTAB                                                       
         LA    RF,MKDPQW-MKTTABD(RE)     ZAP ALL COUNTERS                       
         LA    R0,L'MKDPQW/8                                                    
         ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
*                                                                               
GCP4     DS    0H                                                               
         LA    R7,CPPREC                                                        
         GET   CPPFIL,CPPREC                                                    
         CLI   QOPT5,C' '          TEST TO TRACE                                
         BE    *+8                                                              
         BAS   RE,CPTRACE                                                       
         CLI   CPPREC,C'R'         REQUEST RECORD                               
         BNE   GCP4T                                                            
         MVC   CPPREQ,CPPREC       SAVE IT                                      
         MVC   REQMSG,SPACES                                                    
*                                                                               
         MVC   REQMSG(11),=C'RTG SERVICE'                                       
         CLI   CQRSRV,C' '         RTG SERCICE                                  
         BNH   GCP4R                                                            
         CLC   QOPT1,CQRSRV        MUST AGREE WITH DOWNLOAD REQ                 
         BNE   GCP4S                                                            
         OC    CQTARG,=3C'0'       TARGET DEMO                                  
         PACK  DUB,CQTARG                                                       
         CVB   R0,DUB                                                           
         LA    RF,DEMNTAB          GET DEMO NAME                                
         LA    RE,NDEMS                                                         
*                                                                               
GCP4F    DS    0H                                                               
         CLM   R0,1,12(RF)                                                      
         BE    GCP4H                                                            
         LA    RF,13(RF)                                                        
         BCT   RE,GCP4F                                                         
         MVC   REQMSG,SPACES                                                    
         MVC   REQMSG(4),=C'DEMO'  NEW DEMO                                     
         B     GCP4R                                                            
*                                                                               
GCP4H    DS    0H                                                               
         MVC   TRGDEM,7(RF)        SET TARGET DEMO NAME                         
*                                                                               
         MVC   REQMSG,SPACES                                                    
         MVC   REQMSG(11),=C'CLT/AGY/IND'                                       
         CLI   QOPT3,C'C'          CLIENT DATA                                  
         BNE   GCP4J                                                            
         CLC   CQPROG,=C'42'       MUST BE 42 REQUEST                           
         BNE   GCP4S                                                            
         CLC   CQCLT,=C'ALL'       CLT CANNOT BE ALL                            
         BE    GCP4S                                                            
         B     GCP4Q                                                            
*                                                                               
GCP4J    DS    0H                                                               
         CLI   QOPT3,C'A'          AGENCY DATA                                  
         BE    GCP4Q                                                            
*                                                                               
         CLI   QOPT3,C'I'          INDUSTRY DATA                                
         BNE   GCP4R                                                            
         CLC   CQPROG,=C'32'       MUST BE 32 REQ                               
         BNE   GCP4S                                                            
*                                                                               
GCP4Q    DS    0H                                                               
         B     GCP4                                                             
*                                                                               
GCP4R    DS    0H                                                               
         MVC   P(80),CPPREQ+1                                                   
         MVC   P+82(16),=C'** BAD CPPREQ **'                                    
         MVC   P+99(L'REQMSG),REQMSG                                            
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
GCP4S    DS    0H                                                               
         MVC   P(80),CPPREQ+1                                                   
         MVC   P2(80),QAREA                                                     
         MVC   P3(39),=C'** CPP AND DOWNLOAD REQ INCOMPATIBLE **'               
         MVC   P3+40(L'REQMSG),REQMSG                                           
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
GCP4T    DS    0H                                                               
         CLI   CPPREC,CPTCIDEQ     DATA RECORD                                  
         BNE   GCP4                                                             
         USING CPTC,R7                                                          
*                                                                               
         CLI   QUESTOR,C'0'     MARKET LIMIT FOR TESTING                        
         BL    *+14                                                             
         CLC   QUESTOR(4),CPTCMNO                                               
         BL    GCP4                                                             
*                                                                               
GCP6     DS    0H                                                               
         L     R6,AMKTTAB                                                       
         USING MKTTABD,R6                                                       
         CLC   CPTCMNO,LASTMKT     TEST MKT BREAK                               
         BE    GCP40                                                            
         OC    LASTMKT,LASTMKT     TST FIRST TIME                               
         BZ    GCP20               YES - DONT FINISH PREV MKT                   
*                                                                               
         CLI   SUPRMKT,C'Y'        TEST SUPPRESS MARKET                         
         BE    GCP7                YES, STILL DO US TOTALS                      
         CLI   FLACTSW,C'Y'        TEST MARKET ACTIVE                           
         BNE   GCP7                                                             
         BAS   RE,PUTMKT                                                        
*                                  ROLL TO US TOTALS                            
*                                  -----------------                            
GCP7     DS    0H                                                               
         MVI   FLACTSW,C'N'        RESET ACTIVE MKT SW                          
         L     R3,AUSTTAB          US TOTALS                                    
         LA    R3,MKDPQW-MKTTABD(R3) DPT/Q WORK                                 
         LA    R4,MKDPQW           THIS MKTS DPT/Q AREA                         
         LA    R5,NDPTS*NQTRS                                                   
*                                                                               
GCP16    DS    0H                                                               
         CP    0(8,R4),=P'0'       TEST ANY SPOTS FOR DPT/QTR                   
         BNE   GCP17                                                            
         LA    R3,MKDPDL(R3)       NEXT DPT/Q - US                              
         LA    R4,MKDPDL(R4)       NEXT DPT/Q - MKT                             
         B     GCP19                                                            
*                                                                               
GCP17    DS    0H                                                               
         AP    08(8,R3),08(8,R4)     $                                          
         AP    16(8,R3),16(8,R4)     IMPS                                       
*                                                                               
         CVB   R1,0(8,R4)            SPOTS MUST BE WEIGHTED BY HH POP           
         CVB   RF,MKDPUNVS-MKDPD(R4)                                            
         MR    R0,RF                                                            
         LA    RF,1000                                                          
         BAS   RE,DIV                                                           
         CVD   R1,DUB                                                           
         AP    00(8,R3),DUB        ADD TO US TOTALS                             
*                                                                               
         LA    R3,MKDPGRPS-MKDPD(R3)    FIRST GRP                               
         LA    R4,MKDPGRPS-MKDPD(R4)                                            
         LA    R2,NDEMS            DEMO COUNT FOR BCT                           
*                                                                               
GCP18    DS    0H                                                               
         CVB   R1,0(R4)            MKT GRPS                                     
         CVB   RF,NDEMS*8(R4)      POPS ARE RIGHT AFTER GRPS                    
         MR    R0,RF                                                            
         LA    RF,1000                                                          
         BAS   RE,DIV                                                           
         CVD   R1,DUB                                                           
         AP    0(8,R3),DUB         ADD TO US TOT                                
         AP    NDEMS*8(8,R3),NDEMS*8(8,R4)   ADD POPS                           
*                                                                               
         LA    R3,8(R3)            NEXT ITEM                                    
         LA    R4,8(R4)                                                         
         BCT   R2,GCP18                                                         
*                                                                               
         LA    R3,L'MKDPUNVS(R3)   POSITION TO NEXT DPT/QTR                     
         LA    R4,L'MKDPUNVS(R4)                                                
*                                                                               
GCP19    DS    0H                                                               
         BCT   R5,GCP16            NEXT DPT/Q                                   
*                                                                               
GCP20    DS    0H                  START OF NEW MKT                             
         MVI   SUPRMKT,C'N'                                                     
         CLI   CPTCMNO,X'FF'       TEST EOF                                     
         BE    GCP50                                                            
         MVC   LASTMKT,CPTCMNO                                                  
*                                                                               
         LR    RE,R6               CLEAR MARKET AREA                            
         LA    RF,MKLEN                                                         
         XCEF                                                                   
         LA    RF,MKDPQW           ZAP ALL COUNTERS                             
         LA    R0,L'MKDPQW/8                                                    
         ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         PACK  DUB,CPTCMNO         CPP MKT IS ALWAYS DMA                        
         SP    DUB,=P'400'         SUBTRACT 400 TO GET TRUE DMA                 
         CVB   R0,DUB                                                           
         STCM  R0,3,MKNUM          BINARY MKT NUMBER                            
*                                                                               
         MVC   WORK,SPACES                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         GOTO1 BINSRCH,MKTPARS,WORK                                             
         L     RF,0(R1)                                                         
         CLI   0(R1),1             TEST FOUND                                   
         BNE   GCP24                                                            
*                                                                               
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BO    GCP23               SKIP MESSAGE                                 
         MVC   P(26),=C'** BAD CPP MARKET - NNN **'                             
         MVC   P+20(3),WORK                                                     
         GOTO1 REPORT                                                           
GCP23    DS    0H                                                               
         MVI   SUPRMKT,C'Y'        SUPPRESS MARKET                              
         B     GCP40               BUT INLCUDE IN US TOTALS                     
*                                                                               
GCP24    DS    0H                                                               
         CLI   QOPT1,C'A'          IF DOING ADI'S MUST TRANSLATE                
         BNE   GCP40                                                            
         CLI   3(RF),C' '          IF NOT ADI                                   
         BH    *+12                                                             
         MVI   SUPRMKT,C'Y'        SUPPRESS MARKET                              
         B     GCP40               BUT INCLUDE IN US TOTALS                     
         PACK  DUB,3(3,RF)         ADI                                          
         CVB   R0,DUB                                                           
         STCM  R0,3,MKNUM                                                       
*                                                                               
GCP40    DS    0H                  CONTINUE SAME MKT                            
         LA    RF,DNAMES           SET DEMO IN DNAMES LIST                      
         LA    R0,NDEMS                                                         
GCP41    DS    0H                                                               
         CLI   0(RF),0             EMPLY SLOT?                                  
         BE    GCP42                                                            
         CLC   0(7,RF),CPTCREP                                                  
         BE    GCP43                                                            
         LA    RF,12(RF)                                                        
         BCT   R0,GCP41                                                         
         DC    H'0'                TOO MANY DEMOS                               
GCP42    DS    0H                                                               
         MVC   0(7,RF),CPTCREP                                                  
         L     RE,NADEMS           BUMP ACTIVE DEMO COUNT                       
         LA    RE,1(RE)                                                         
         ST    RE,NADEMS                                                        
*                                  GET HMS DEMO NAME                            
         LA    RE,DEMNTAB                                                       
*                                                                               
GCP42B   DS    0H                                                               
         CLI   0(RE),X'FF'         EOL                                          
         BNE   *+6                                                              
         DC    H'0'                NEW DEMO                                     
         CLC   0(7,RF),0(RE)                                                    
         BE    *+12                                                             
         LA    RE,13(RE)                                                        
         B     GCP42B                                                           
*                                                                               
         MVC   7(5,RF),7(RE)       SET EXTRACT NAME                             
*                                                                               
GCP43    DS    0H                                                               
         LCR   R0,R0                                                            
         AH    R0,=Y(NDEMS)                                                     
         STC   R0,DEMNO            SET DEMO SEQ NUMBER                          
*                                                                               
         BAS   RE,SETDPT           SET DAYPART NUMBER                           
         CLI   DPTNO,0             SKIP IF NOT TO BE USED                       
         BE    GCP4                                                             
*                                                                               
         MVI   QTRNO,0             'TOTAL' QTR ONLY                             
         BAS   RE,SETQDP                                                        
         B     GCP4                                                             
*                                                                               
GCP50    DS    0H                  DONE WITH MKTS, NOW DO US TOTALS             
*                                  FIRST COMPUTE GRPS FROM AUDS                 
         L     R6,AUSTTAB                                                       
         LA    R5,NDPTS*NQTRS                                                   
         LA    R3,MKDPQW                                                        
         USING MKDPD,R3                                                         
*                                                                               
GCP52    DS    0H                                                               
         LA    R4,MKDPGRPS                                                      
         LA    R2,NDEMS                                                         
*                                                                               
GCP54    DS    0H                                                               
         CVB   R1,0(R4)            AUD                                          
         M     R0,=F'1000'                                                      
         CVB   RF,NDEMS*8(R4)      / POP                                        
         BAS   RE,DIV                                                           
         CVD   R1,0(R4)            RESET GRP                                    
*                                                                               
         LA    R4,8(R4)                                                         
         BCT   R2,GCP54                                                         
*                                                                               
         CVB   R1,MKDPSPT          SPOTS MUST BE DIVIDED                        
         CVB   RF,MKDPUNVS         BY HH POP                                    
         M     R0,=F'1000'                                                      
         BAS   RE,DIV                                                           
         CVD   R1,MKDPSPT                                                       
*                                                                               
         LA    R3,MKDPDL(R3)        NEXT DPT/QTR                                
         BCT   R5,GCP52                                                         
*                                                                               
         BAS   RE,PUTMKT           DO DRIVER INPUT FOR US TOTALS                
         B     GCPX                                                             
*                                                                               
CPPEOF   DS    0H                                                               
         CLOSE CPPFIL                                                           
         MVI   CPTCMNO,X'FF'                                                    
         B     GCP6                                                             
*                                                                               
GCPX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        SETQDP - SET DATA FOR QTR/DPT                                          
***********************************************************************         
         SPACE 2                                                                
SETQDP   NTR1                                                                   
         LA    RF,CPTCAS           USE FIRST SET OF DATA                        
         CLI   QOPT3,C'C'          FOR CLIENT REQ                               
         BE    SQDP3                                                            
         CLI   QOPT3,C'A'          OR AGY REQ AND                               
         BNE   SQDP2                                                            
         CLC   CQPROG,=C'32'       AGY/IND CPP REQ                              
         BE    SQDP3                                                            
*                                                                               
SQDP2    DS    0H                                                               
         LA    RF,CPTCIS           ELSE USE 2ND SET OF DATA                     
*                                                                               
SQDP3    DS    0H                                                               
         MVC   CPFLDS,0(RF)                                                     
*                                                                               
         CP    CPSPOTS,=P'0'       TEST ANY SPOTS                               
         BE    SQDPX               NO - DONE                                    
         CP    CPDOLLS,=P'0'       AND DOLLARS                                  
         BE    SQDPX               NO - DONE                                    
         MVI   FLACTSW,C'Y'        SET HAVE ACTIVITY FOR DL FILE                
*                                                                               
         ZIC   RF,DPTNO            DISPL = DPTNO                                
         BCTR  RF,R0                                                            
         MH    RF,=Y(NQTRS)                X MAX QTRS                           
         ZIC   RE,QTRNO                    + QTRNO                              
         AR    RF,RE                                                            
         MH    RF,=Y(MKDPDL)               X ENTRY LENGTH                       
         LA    R6,MKDPQW-MKTTABD(R6)                                            
         AR    R6,RF               POINT TO RIGHT ENTRY                         
         USING MKDPD,R6                                                         
*                                                                               
         CLI   DEMNO,0             FOR HOMES                                    
         BNE   SQDP6                                                            
         AP    MKDPSPT,CPSPOTS     SPOTS                                        
         AP    MKDPCST,CPEQDLS     EQUIV DOLLARS                                
         AP    MKDPIMP,CPIMPS      ADD IMPS                                     
SQDP6    DS    0H                                                               
         ZIC   RF,DEMNO            SET THIS DEMO GRPS                           
         SLL   RF,3                X 8 BYTES PER DEMO                           
         LA    RF,MKDPGRPS(RF)                                                  
         AP    0(8,RF),CPGRPS                                                   
*                                                                               
         ZIC   RF,DEMNO            SET UNIVERSES                                
         SLL   RF,3                                                             
         LA    RF,MKDPUNVS(RF)                                                  
         ZAP   0(8,RF),CPTCUNIV                                                 
*                                                                               
SQDPX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        PUTMKT - PUT MARKET DATA TO DRIVER                                     
***********************************************************************         
         SPACE 2                                                                
PUTMKT   NTR1                                                                   
         L     RF,AMKTCNT          BUMP ACTIVE MKT COUNT                        
         LA    RF,1(RF)                                                         
         ST    RF,AMKTCNT                                                       
         MVI   DRPASS,1            MKT TOTAL PASS                               
         MVI   QTRNO,0             DO TOTAL QUARTER ONLY                        
         MVI   GLMODE,GLINPUT                                                   
         MVI   ACTRECSW,0          CLEAR ACTIVE SW                              
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         CLC   NADEMS,=F'1'        MUST HAVE DEMOS OTHER THAN HH                
         BL    PMKTX               TO DO DCF PASS                               
         TM    GLDOWNLD,X'80'                                                   
         BZ    PMKTX                                                            
         MVI   DRPASS,C'D'                                                      
         LA    R3,1                FIRST DEMO                                   
*                                                                               
PMTK4    DS    0H                                                               
         STC   R3,DEMNO                                                         
         MVI   GLMODE,GLINPUT                                                   
         MVI   ACTRECSW,0          CLEAR ACTIVE SW                              
         GOTO1 ADRIVER,DMCB,(RC)                                                
         LA    R3,1(R3)            NEXT DEMO                                    
         C     R3,NADEMS                                                        
         BNH   PMTK4                                                            
*                                                                               
PMKTX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        SETDPT - SET DOWNLOAD DAYPART                                          
***********************************************************************         
         SPACE 2                                                                
SETDPT   DS    0H                                                               
         MVI   DPTNO,0                                                          
         CLI   CPTCPTY,4           ALL SPORTS PROGS                             
         BNE   *+10                                                             
         MVI   DPTNO,9             IN SPORTS DAYPART                            
         BR    RE                                                               
         CLI   CPTCPTY,5           ALL KIDS PROGS                               
         BNE   *+10                                                             
         MVI   DPTNO,10            IN KIDS DAYPART                              
         BR    RE                                                               
*                                                                               
         LA    RF,NEWSDPL          NEWS PROGRAM DPT LIST                        
         CLI   CPTCPTY,6                                                        
         BE    SDPT4                                                            
         LA    RF,REGDPL           OR REGULAR PROG                              
         CLI   CPTCPTY,1           ALL REG                                      
         BE    SDPT4                                                            
         CLI   CPTCPTY,2           REG/NETWORK                                  
         BE    SDPT4                                                            
         CLI   CPTCPTY,3           REG/IND                                      
         BE    SDPT4                                                            
         DC    H'0'                BAD PROGRAM TYPE                             
*                                                                               
SDPT4    DS    0H                                                               
         CLI   0(RF),X'FF'         EOL                                          
         BNE   *+6                                                              
         DC    H'0'                BAD DAYPART                                  
         CLC   0(1,RF),CPTCDPT                                                  
         BE    SDPT6                                                            
         LA    RF,2(RF)                                                         
         B     SDPT4                                                            
*                                                                               
SDPT6    DS    0H                                                               
         MVC   DPTNO,1(RF)                                                      
         BR    RE                                                               
         EJECT                                                                  
*        BUILD DMA/ADI TRANSLATE TABLE                                          
         SPACE 2                                                                
BLDMTAB  NTR1                                                                   
         MVC   MKTSAV+40(8),=C'DMALIST '   ALWAYS READ DMAS                     
*                                                                               
         OPEN  (MKTSAV,(INPUT))                                                 
*                                  SET BINSRCH PARS                             
         SR    R0,R0                                                            
         L     R1,=A(MTAB)                                                      
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,36               DMA,ADI,NAME                                 
         LA    R4,3                                                             
         LH    R5,=Y(MKTMAX)                                                    
         STM   R0,R5,MKTPARS                                                    
*                                                                               
BMK4     DS    0H                                                               
         GET   MKTSAV,MW                                                        
         CLI   MW,C'*'             SKIP COMMENTS                                
         BE    BMK4                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),MW+MSRNUM-MSRECD                                         
         MVC   WORK+3(3),MW+MSRADI-MSRECD                                       
         MVC   WORK+6(30),MW+MSRNAM-MSRECD                                      
         GOTO1 BINSRCH,MKTPARS,(1,WORK)                                         
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     BMK4                NEXT MARKET                                  
*                                                                               
BMEOF    DS    0H                                                               
         CLOSE MKTSAV                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                  TRACE CPP RECORDS                            
*                                                                               
CPTRACE  NTR1                                                                   
         LA    R7,CPPREC                                                        
         USING CPTC,R7                                                          
         CLI   CPTCID,C'1'                                                      
         BNL   CPTR4                                                            
         MVC   P(CPTCLEN),CPTC                                                  
         GOTO1 REPORT                                                           
         B     CPTRX                                                            
*                                                                               
CPTR4    DS    0H                                                               
         CLI   QUESTOR,C'0'     MARKET LIMIT FOR TESTING                        
         BL    *+14                                                             
         CLC   QUESTOR(4),CPTCMNO                                               
         BL    CPTRX                                                            
         CLC   CPTCRS,QOPT1        TEST RIGHT SERVICE                           
         BNE   CPTRX                                                            
         MVC   P(20),CPTC                                                       
         MVC   P+21(1),CPTCDPT                                                  
         EDIT  (B1,CPTCPTY),(3,P+22),ZERO=NOBLANK                               
         MVI   P+25,C'='                                                        
         BAS   RE,SETDPT           SKIP DPTS/PROGS NOT TO BE USED               
         EDIT  (B1,DPTNO),(3,P+26),ZERO=NOBLANK                                 
         EDIT  (P8,CPTCUNIV),(11,P+29),ZERO=NOBLANK                             
*                                                                               
         LA    R2,5                                                             
         LA    R3,CPTCAS           USE FIRST SET OF DATA                        
         CLI   QOPT3,C'C'          FOR CLIENT REQ                               
         BE    CPTR5B                                                           
         CLI   QOPT3,C'A'          OR AGY REQ AND                               
         BNE   CPTR5                                                            
         CLC   CQPROG,=C'32'       AGY/IND CPP REQ                              
         BE    CPTR5B                                                           
*                                                                               
CPTR5    DS    0H                                                               
         LA    R3,CPTCIS           ELSE USE 2ND SET OF DATA                     
*                                                                               
CPTR5B   DS    0H                                                               
         LA    R4,P+40                                                          
*                                                                               
CPTR6    DS    0H                                                               
         EDIT  (P8,0(R3)),(11,0(R4)),ZERO=NOBLANK                               
         LA    R3,8(R3)                                                         
         LA    R4,12(R4)                                                        
         BCT   R2,CPTR6                                                         
*                                                                               
         GOTO1 REPORT                                                           
CPTRX    DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
MSRECD   DSECT                                                                  
MSRNAM   DS    CL30                                                             
MSRNUM   DS    CL3                                                              
MSRTZ    DS    CL1                                                              
MSRCLS   DS    CL1                                                              
MSRREG   DS    CL2                                                              
MSRABBR  DS    CL8                                                              
MSRADI   DS    CL3                 ADI EQUIVALENT                               
         DS    CL29                SPARE                                        
         SPACE 2                                                                
SPPW02   CSECT                                                                  
MKTSAV   DCB   DDNAME=MKTSAV,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=BMEOF                                                      
*                                                                               
CPPFIL   DCB   DDNAME=CPPFIL,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=CPPEOF            
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
DIV      DIV   (R0),(RF)       RETURNS VIA RE                                   
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
*        DEMO NAME TRANSLATION  LIST                                            
*                                                                               
DEMNTAB  DS    0X                                                               
         DC    CL12'HOMES  HOMES',AL1(001)                                      
         DC    CL12'WM18-34W1834',AL1(041)                                      
         DC    CL12'WM25-54W2554',AL1(048)                                      
         DC    CL12'MN18-34M1834',AL1(091)                                      
         DC    CL12'MN25-54M2554',AL1(098)                                      
         DC    CL12'CH2-11 V0211',AL1(122)                                      
         DC    CL12'TEENS  V1217',AL1(125)                                      
         DC    CL12'VW12-34V1234',AL1(129)                                      
         DC    CL12'AD18-34A1834',AL1(141)                                      
         DC    CL12'AD25-54A2554',AL1(148)                                      
         DC    CL12'WM18-49W1849',AL1(042)                                      
         DC    CL12'MN18-49M1849',AL1(092)                                      
         DC    CL12'AD18-49A1849',AL1(142)                                      
NDEMS    EQU   (*-DEMNTAB)/13                                                   
*                                                                               
         DC    3X'FF'                                                           
         SPACE 3                                                                
*        DAYPART NAME LIST                                                      
*                                                                               
DPTLST   DS    0X                                                               
         DC    CL2'PR'    1                                                     
         DC    CL2'EM'    2                                                     
         DC    CL2'DA'    3                                                     
         DC    CL2'EF'    4                                                     
         DC    CL2'EN'    5                                                     
         DC    CL2'PA'    6                                                     
         DC    CL2'LN'    7                                                     
         DC    CL2'LF'    8                                                     
         DC    CL2'SP'    9                                                     
         DC    CL2'CH'    10                                                    
NDPTS    EQU   (*-DPTLST)/2                                                     
*                                                                               
         DC    3X'FF'                                                           
         SPACE 3                                                                
*        CPP DAYPART/PROGRAM TRANSATION LIST - REGULAR PROGS                    
*                                                                               
REGDPL   DS    0X                                                               
*                               CPP DPT    EXTRACT DPT                          
*                               -------    -----------                          
         DC    C'A',AL1(02)     EAM        EM                                   
         DC    C'C',AL1(03)     DAY        DA                                   
         DC    C'E',AL1(02)     WEM        EM                                   
         DC    C'G',AL1(03)     WEA        DA                                   
         DC    C'J',AL1(04)     ELY        EF                                   
         DC    C'H',AL1(04)     FRINGE?    EF                                   
         DC    C'K',AL1(00)     SUN PRI    *   (USE PRIME COMBINED)             
         DC    C'N',AL1(00)     PRI        *           ''                       
         DC    C'L',AL1(06)     PAC        PA                                   
         DC    C'P',AL1(00)     LTE        *   (USE LATE/LTE-LTE COMB)          
         DC    C'R',AL1(00)     LLT        *            ''                      
         DC    C'1',AL1(01)     PRI+SUP    PR                                   
         DC    C'2',AL1(08)     LTE+LLT    LF                                   
         DC    C'7',AL1(00)     NON-PRIME  *                                    
         DC    C'8',AL1(00)     TOTAL      *                                    
         DC    C'9',AL1(00)     VARIOUS    *                                    
*                                                                               
         DC    3X'FF'                                                           
         SPACE 3                                                                
*        CPP DAYPART/PROGRAM TRANSATION LIST - NEWS PROGS                       
*                                                                               
NEWSDPL  DS    0X                                                               
*                               CPP DPT    EXTRACT DPT                          
*                               -------    -----------                          
         DC    C'A',AL1(02)     EAM        EM                                   
         DC    C'C',AL1(03)     DAY        DA                                   
         DC    C'E',AL1(02)     WEM        EM                                   
         DC    C'G',AL1(03)     WEA        DA                                   
         DC    C'J',AL1(05)     ELY        EN                                   
         DC    C'H',AL1(05)     FRINGE?    EN                                   
         DC    C'K',AL1(01)     SUN PRI    PR                                   
         DC    C'N',AL1(01)     PRI        PR                                   
         DC    C'L',AL1(05)     PAC        EN                                   
         DC    C'P',AL1(07)     LTE        LN                                   
         DC    C'R',AL1(07)     LLT        LN                                   
         DC    C'1',AL1(00)     PRI+SUP    *                                    
         DC    C'2',AL1(00)     LTE+LLT    *                                    
         DC    C'7',AL1(00)     NON-PRIME  *                                    
         DC    C'8',AL1(00)     TOTAL      *                                    
         DC    C'9',AL1(00)     VARIOUS    *                                    
*                                                                               
         DC    3X'FF'                                                           
*                                                                               
NQTRS    EQU   1                  MAX QUARTERS INCLUDING 'TOTAL'                
MKTMAX   EQU   300                                                              
         SPACE 3                                                                
RELO     DC    F'0'                                                             
RSMKT    DS    H                                                                
SVADMKT  DS    A                                                                
VGLAREA  DS    A                                                                
AMKTTAB  DS    A                                                                
AUSTTAB  DS    A                                                                
X        DS    XL256                                                            
SRCE     DS    C                                                                
TYPRET   DS    C                                                                
         DS    0D                                                               
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
*                                                                               
DPGFILE  DC    CL8'SPPW05  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
XFF      DC    20X'FF'                                                          
*                                                                               
SVKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
DEMTRCE  DS    CL1                                                              
ANXTMSL  DS    F                                                                
POLPRD   DS    A                                                                
MKTCTR   DS    H                                                                
MKTTRC   DS    H                                                                
STIM     DS    H                                                                
BINDAY   DS    X                                                                
DRPASS   DS    X                                                                
ESTFSW   DS    C                                                                
REQFSW   DS    C                                                                
DLHSW    DS    C                                                                
ACTRECSW DS    X                                                                
DPTNO    DS    X                                                                
QTRNO    DS    X                                                                
DEMNO    DS    X                                                                
MDQIDX   DS    F                                                                
QTRTAB   DS    XL6                                                              
MKACTSW  DS    X                                                                
FLACTSW  DS    X                                                                
MKTPARS  DS    6F                                                               
AMTAB    EQU   MKTPARS+4                                                        
SUPRMKT  DS    XL1                                                              
LASTMKT  DC    XL4'00'                                                          
TRGDEM   DC    XL5'00'                                                          
AMKTCNT  DC    F'0'                                                             
NADEMS   DS    F'0'                                                             
REQMSG   DS    CL20                                                             
*                                                                               
CPFLDS   DS    0XL40               FIELDS MOVED FROM CPP TAPE                   
CPSPOTS  DS    PL8                                                              
CPDOLLS  DS    PL8                                                              
CPEQDLS  DS    PL8                                                              
CPGRPS   DS    PL8                                                              
CPIMPS   DS    PL8                                                              
*                                                                               
MW       DS    CL100                                                            
         SPACE 3                                                                
ROUTLIST DS    0F                                                               
         DC    C'MKTNUM  ',A(MKTNUM)                                            
         DC    C'MKTNAM  ',A(MKTNAM)                                            
         DC    C'MKTPOP  ',A(MKTPOP)                                            
         DC    C'STATIP  ',A(STATIP)                                            
         DC    C'STUBIP  ',A(STUBIP)                                            
         DC    C'DLHQTR  ',A(DLHQTR)                                            
         DC    C'SPTSIP  ',A(SPTSIP)                                            
         DC    C'COSTIP  ',A(COSTIP)                                            
         DC    C'IMPSIP  ',A(IMPSIP)                                            
         DC    C'GRPSIP  ',A(GRPSIP)                                            
         DC    C'DCFIN   ',A(DCFIN)                                             
         DC    C'DLHTTL  ',A(DLHTTL)                                            
         DC    C'DLHSRC  ',A(DLHSRC)                                            
         DC    C'DLHRUND ',A(DLHRUND)                                           
         DC    C'DLHRQID ',A(DLHRQID)                                           
         DC    C'DLHADV  ',A(DLHADV)                                            
         DC    C'DLHPRD  ',A(DLHPRD)                                            
         DC    C'DLHEST  ',A(DLHEST)                                            
         DC    C'DLHSTD  ',A(DLHSTD)                                            
         DC    C'DLHEND  ',A(DLHEND)                                            
         DC    C'DLHNDEM ',A(DLHNDEM)                                           
         DC    C'DLHNMKT ',A(DLHNMKT)                                           
         DC    C'DLHNMKTO',A(DLHNMKTO)                                          
         DC    C'DLHNDPT ',A(DLHNDPT)                                           
         DC    C'DLHDEML ',A(DLHDEML)                                           
         DC    C'DLHDPTL ',A(DLHDPTL)                                           
         DC    C'DPTNAM  ',A(DPTNAM)                                            
         DC    C'DEMNAM  ',A(DEMNAM)                                            
         DC    C'DEMNUM  ',A(DEMNUM)                                            
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
MKTTABD  DSECT                                                                  
MKNUM    DS    XL2                                                              
MKKLEN   EQU   *-MKTTABD                                                        
MKNAM    DS    CL30                                                             
MKUNVS   DS    (NDEMS)PL8                                                       
MKDPQW   DS    XL(MKDPDL*NDPTS*NQTRS)                                           
         DS    0F                                                               
MKLEN    EQU   *-MKTTABD                                                        
*                                                                               
         SPACE 2                                                                
*                                                                               
MKDPD    DSECT                     DATA FOR A MKT/DPT/QTR                       
MKDPSPT  DS    PL8                 SPOTS                                        
MKDPCST  DS    PL8                 COST                                         
MKDPIMP  DS    PL8                 HH IMPS                                      
MKDPGRPS DS    XL(NDEMS*8)         GRPS - HH FIRST                              
MKDPUNVS DS    XL(NDEMS*8)         UNIVERSES                                    
MKDPDL   EQU   *-MKDPD                                                          
*                                                                               
SPPW02   CSECT                                                                  
         DS    0D                                                               
DNAMES   DC    C'HOMES  HOMES'       CPP NAME(7), EXTRACT NAME(5)               
         DS    XL((NDEMS-1)*(7+5))                                              
         DS    0D                                                               
CPPREC   DS    XL(CPTCLEN)         CPPRS RECORD                                 
CPPREQ   DS    XL(CPTCLEN)         CPP REQUEST                                  
         ORG   CPPREQ                                                           
         DS    CL1                 1 - RECORD TYPE                              
CQPROG   DS    CL2                                                              
         DS    CL4                                                              
CQCLT    DS    CL3                 7 - CLIENT                                   
         DS    CL22                                                             
CQSTAM   DS    CL4                 32 - START                                   
CQENDM   DS    CL4                 36 - END                                     
         DS    CL4                                                              
CQTARG   DS    CL3                 44 - TARGET                                  
         DS    CL4                                                              
CQRSRV   DS    CL1                 51 - RATING SERVICE                          
         ORG                                                                    
         DS    0D                                                               
MKTTAB   DS    XL(MKLEN)                                                        
         DS    0D                                                               
USTTAB   DS    XL(MKLEN)                                                        
         DS    0D                                                               
MTAB     DS    XL(36*MKTMAX)                                                    
         DS    0D                                                               
         DC    CL8'DRGLOBAL'                                                    
GLAREA   DS    40000X              DRIVER GLOBAL AREA                           
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         SPACE 2                                                                
       ++INCLUDE SPDRVWRKD                                                      
         SPACE 2                                                                
       ++INCLUDE CPTAPED                                                        
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
DBLKD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
       ++INCLUDE SPMEDBLOCK                                                     
         SPACE 2                                                                
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065SPREPPW02 05/01/02'                                      
         END                                                                    
