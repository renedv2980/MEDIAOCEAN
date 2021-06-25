*          DATA SET SPREPXL02  AT LEVEL 005 AS OF 05/01/02                      
*PHASE SPXL02A                                                                  
         TITLE 'SPREPXL02 - BUYER WORKLOAD ANALYSIS'                            
         PRINT NOGEN                                                            
SPXL02   CSECT                                                                  
         NMOD1 0,SPXL02                                                         
*                                                                               
         L     R8,0(R1)                                                         
         LA    R9,2048(R8)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,R8,R9                                                    
*                                                                               
         L     R7,=A(GLAREA)                                                    
         USING GLOBALD,R7                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SP010                                                            
         CLI   MODE,REQFRST                                                     
         BE    SP100                                                            
         CLI   MODE,CLTFRST                                                     
         BE    SP200                                                            
         CLI   MODE,REQLAST                                                     
         BE    SP700                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* RUN FIRST                                                                     
*                                                                               
SP010    XC    ADRIVER,ADRIVER                                                  
         XC    ASYSDRV,ASYSDRV                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* REQUEST FIRST                                                                 
*                                                                               
SP100    MVC   PAGE,=X'0001'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   ANYDATA,C'N'        NO DATA YET                                  
         MVI   PRTSW,C'Y'                                                       
         MVI   TOTLINE,C'N'                                                     
         MVI   FIRSTCLT,C'Y'                                                    
*                                                                               
         LA    RE,GLOBALD          INIT DRIVER STORAGE TO ZEROS                 
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         MVC   GLSIZE,=F'40000'    GLOBAL SIZE                                  
         MVC   GLOBALD(8),=C'*GLOBAL*'                                          
*                                                                               
         OC    GLAPROG,GLAPROG                                                  
         BZ    SP105                                                            
         SR    R3,R3                                                            
         BCTR  R3,0                                                             
         GOTO1 LOADER,DMCB,DPGFILE,(R3)                                         
*                                                                               
SP105    GOTO1 LOADER,DMCB,DPGFILE,0    LOAD DPG PROGRAM                        
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,GLAPROG                                                       
*                                                                               
         OC    ADRIVER,ADRIVER                                                  
         BNZ   SP110                                                            
         GOTO1 LOADER,DMCB,DRIVER       LOAD DRIVER                             
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ADRIVER                                                       
*                                                                               
SP110    OC    ASYSDRV,ASYSDRV                                                  
         BNZ   SP115                                                            
         GOTO1 LOADER,DMCB,SPDRIVER     LOAD SYSTEM DRIVER                      
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ASYSDRV                                                       
*                                                                               
SP115    MVC   GLASYSDR,ASYSDRV    SYSTEM DRIVER ADDR                           
*                                                                               
         LA    RE,SPWORKD                                                       
         ST    RE,GLAWORKD         SPOT WORK ADDR                               
         LA    RE,DRHOOK                                                        
         ST    RE,GLAHOOK          OUR DRIVER HOOK                              
         MVI   GLTWORKD,GLTSPOT    SPOT WORK                                    
         MVI   GLAUTOCH,C'N'       NO AUTOCHUNKING                              
         MVI   GLDETHED,C'Y'       I GET HEADS AT DETAIL TIME                   
         MVI   GLFHEADL,11                                                      
         MVI   GLLHEADL,12                                                      
*                                                                               
         XC    QBMKT,QBMKT         DETERMINE MARKET FILTERING                   
         CLC   =C'ALL',QMKT                                                     
         BE    SP117                                                            
         CLC   QMKT,SPACES                                                      
         BE    SP117                                                            
         PACK  DUB,QMKT                                                         
         CVB   RE,DUB                                                           
         STCM  RE,3,QBMKT                                                       
         B     SP120                                                            
*                                                                               
SP117    CLI   QSTA,C'0'           DETERMINE MARKET GROUP FILTERING             
         BL    SP120                                                            
         LA    R1,QSTA+3                                                        
         LA    R0,3                                                             
*                                                                               
SP118    CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,SP118                                                         
         STH   R0,QMGRLEN                                                       
         B     SP120                                                            
         EJECT                                                                  
*                                                                               
* MARKET GROUP ASSIGN TABLE                                                     
*                                                                               
SP120    LA    R0,20000/250        CLEAR MARKET GROUP TABLE                     
         L     R1,AMGRTAB                                                       
*                                                                               
SP125    MVI   0(R1),X'99'                                                      
         MVC   1(249,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,SP125                                                         
*                                                                               
         CLI   QMGR,C' '           MUST BE MKTGRP REQUEST                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             BUILD MARKET GROUP ASSIGN TABLE              
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),BAGYMD     A-M                                          
         MVC   KEY+8(1),QMGR                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    SP145                                                            
         DC    H'0'                NO MARKETS ASSIGNED                          
*                                                                               
SP130    GOTO1 SEQ                                                              
*                                                                               
SP140    CLC   KEY(9),KEYSAVE                                                   
         BNE   SP150                                                            
*                                                                               
SP145    SR    RE,RE                                                            
         ICM   RE,3,KEY+11         GET MKT NUM                                  
         AR    RE,RE               X 2                                          
         A     RE,AMGRTAB          + TABLE START                                
         MVC   0(2,RE),KEY+9       MOVE MKTGRP NUMBER                           
         B     SP130                                                            
*                                                                               
SP150    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* CLIENT FIRST                                                                  
*                                                                               
SP200    CLI   FIRSTCLT,C'Y'       FOR FIRST CLIENT ONLY -                      
         BNE   SP300                                                            
         MVI   FIRSTCLT,C'N'                                                    
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 ADRIVER,DMCB,(R7)                                                
*                                                                               
         L     RE,GLAPLIST                                                      
         MVC   ASPDRWKC,0(RE)      A(SPOT DRIVER WORK AREA)                     
*                                                                               
         ZIC   RE,SDNMGRPS         DETERMINE LEVELS                             
         LA    RE,1(RE)                                                         
         STC   RE,MKTLEV                                                        
         EJECT                                                                  
*                                                                               
* PRODUCT/ESTIMATE TABLE                                                        
*                                                                               
SP300    L     R1,AESTTAB          CLEAR ESTIMATE TABLE                         
         LA    R0,256                                                           
*                                                                               
SP350    XC    0(256,R1),0(R1)     CLEAR 256 BYTES FOR EACH BRAND               
         LA    R1,256(R1)                                                       
         BCT   R0,SP350                                                         
*                                                                               
         L     R6,ADCLT                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(4),0(R6)                                                     
         MVI   KEY+4,C'A'          FORCE PAST CLTHDR                            
         GOTO1 HIGH                PRODUCT HEADER                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   EXIT                                                             
         B     SP362                                                            
*                                                                               
SP355    GOTO1 HIGH                                                             
         B     SP360                                                            
*                                                                               
SP357    GOTO1 SEQ                                                              
*                                                                               
SP360    CLC   KEY(4),KEYSAVE                                                   
         BNE   SP400                                                            
*                                                                               
SP362    OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   SP357               YES                                          
         CLI   KEY+7,0             TEST EST                                     
         BE    SP357                                                            
*                                                                               
SP375    GOTO1 GETEST                                                           
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   EEND,QSTART         EST END BEFORE REQ START                     
         BL    SP380                                                            
         CLC   ESTART,QEND         EST START AFTER REQ END                      
         BH    SP380                                                            
*                                                                               
         ZIC   RE,EPRDCD+1         GET PRD NUM                                  
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         ZIC   R0,7(R6)            EST NUM                                      
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         A     RE,AESTTAB                                                       
         MVI   0(RE),C'Y'                                                       
*                                                                               
SP380    MVC   KEY+8(5),XFF        NEXT ESTIMATE                                
         B     SP355                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* READ BUY RECORDS                                                              
*                                                                               
SP400    XC    KEY,KEY                                                          
         MVI   DMOUTBTS,X'FD'      DMOUTBTS - PASS DELETES                      
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)                                                     
*                                                                               
SP410    GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      TEST END OF CLIENT                           
         BNE   SP500                                                            
         CLC   KEY(4),KEYSAVE      TEST FOR NEW PRODUCT                         
         BE    SP414                                                            
         CLI   KEY+3,X'FF'         YES - TEST FOR PRODUCT POL                   
         BE    SP500                     YES - FINISHED                         
         MVC   BPRD,KEY+3                                                       
         MVC   PRD,=C'POL'                                                      
         CLI   BPRD,219                                                         
         BNL   SP414                                                            
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
SP412    CLC   BPRD,3(RF)                                                       
         BE    SP413                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNL   SP412                                                            
         DC    H'0'                                                             
*                                                                               
SP413    MVC   PRD,0(RF)           ALPHA PRODUCT CODE                           
*                                                                               
SP414    CLC   KEY(6),KEYSAVE      TEST FOR NEW MARKET                          
         BE    SP418                                                            
         OC    QBMKT,QBMKT         TEST FOR MARKET FILTER                       
         BZ    SP415                                                            
         CLC   QBMKT,KEY+4                                                      
         BE    SP416                                                            
         BL    SP430                                                            
         MVC   KEY+4(2),QBMKT                                                   
         XC    KEY+6(7),KEY+6                                                   
         B     SP410                                                            
*                                                                               
SP415    CLI   QSTA,C'0'           TEST FOR MARKET GROUP FILTER                 
         BL    SP416                                                            
         SR    RE,RE                                                            
         ICM   RE,3,KEY+4                                                       
         AR    RE,RE                                                            
         A     RE,AMGRTAB                                                       
         UNPK  DUB,0(3,RE)                                                      
         LH    RE,QMGRLEN                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   QSTA(0),DUB+3                                                    
         BNE   SP425                                                            
*                                                                               
SP416    XC    DONESTS,DONESTS     CLEAR ESTIMATES DONE                         
         SR    RE,RE               SAVE THE MARKET                              
         ICM   RE,3,KEY+4                                                       
         CVD   RE,DUB                                                           
         UNPK  MKT(4),DUB                                                       
         OI    MKT+3,X'F0'                                                      
         AR    RE,RE                     GET THE MARKET GROUP                   
         A     RE,AMGRTAB                                                       
         MVC   BMGR+1(2),0(RE)                                                  
*                                                                               
SP418    ZIC   RE,KEY+3            PRODUCT                                      
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         ZIC   R0,KEY+9            ESTIMATE                                     
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         A     RE,AESTTAB                                                       
         CLI   0(RE),0             TEST ACTIVE                                  
         BZ    SP420               NO - NEXT EST                                
         LR    RE,R0               YES- TEST EST ALREADY DONE FOR THIS          
         LA    RE,DONESTS(RE)                                 MARKET            
         CLI   0(RE),0                                                          
         BNE   SP420                     YES - NEXT EST                         
         MVI   0(RE),C'Y'                                                       
         MVC   BEST,KEY+9          SET THE ESTIMATE                             
         BAS   RE,PUTDRIV          PUT A DRIVER RECORD                          
*                                                                               
SP420    MVC   KEY+10(3),XFF       NEXT ESTIMATE                                
         B     SP410                                                            
*                                                                               
SP425    MVC   KEY+6(7),XFF        NEXT MARKET                                  
         B     SP410                                                            
*                                                                               
SP430    MVC   KEY+4(9),XFF        NEXT PRODUCT                                 
         B     SP410                                                            
*                                                                               
SP500    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* REQLAST - OUTPUT STAGE                                                        
*                                                                               
SP700    CLI   ANYDATA,C'Y'        TEST FOR ANY DATA                            
         BNE   EXIT                NO - EXIT                                    
         MVI   GLMODE,GLOUTPUT     DRIVER OUTPUT PHASE                          
         GOTO1 ADRIVER,DMCB,(R7)                                                
         MVI   MODE,REQLAST        RESTORE THE MODE                             
         B     EXIT                                                             
         EJECT                                                                  
DRHOOK   NTR1                                                                   
*                                                                               
*        DRIVER HOOK ROUTINE                                                    
*                                                                               
         CLI   GLHOOK,GLROUT       TEST TO EXECUTE USER ROUTINE                 
         BNE   DH005                                                            
         CLI   GLMODE,GLINPUT      YES - INPUT PHASE                            
         BE    DH100                                                            
         CLI   GLMODE,GLOUTPUT           OUTPUT PHASE                           
         BE    DH200                                                            
         DC    H'0'                                                             
*                                                                               
DH005    CLI   GLHOOK,GLPRINT      TEST ABOUT TO PRINT A LINE                   
         BNE   DH007                                                            
         CLI   PRTSW,C'N'          YES - TEST FOR PRINT SUPPRESS                
         BNE   EXIT                                                             
         MVI   GLHOOK,GLDONT                                                    
         MVI   PRTSW,C'Y'                                                       
         B     EXIT                                                             
*                                                                               
DH007    CLI   GLHOOK,GLRESOLV     TEST TO RESOLVE LABELS                       
         BE    DH010                                                            
         CLI   GLHOOK,GLLAST                                                    
         BE    DH300                                                            
         CLI   GLHOOK,GLFIRST                                                   
         BE    DH400                                                            
         CLI   GLHOOK,GLHEAD                                                    
         BE    DH500                                                            
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
ROUTLIST DS    0F                                                               
         DC    C'TOTESTI ',A(TOTESTI)                                           
         DC    C'TOTESTO ',A(TOTESTO)                                           
         DC    C'ESTIM   ',A(ESTIM)                                             
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* EXECUTE INPUT PHASE ROUTINES                                                  
*                                                                               
DH100    L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* EXECUTE OUTPUT PHASE ROUTINES                                                 
*                                                                               
DH200    L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* LAST TIME ROUTINE                                                             
*                                                                               
DH300    CLC   MKTLEV,GLARGS       TEST FOR MARKET LAST OR HIGHER               
         BL    EXIT                                                             
         MVI   TOTLINE,C'Y'        YES - PRINT TOTAL ESTIMATES                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* FIRST TIME ROUTINE                                                            
*                                                                               
DH400    CLC   MKTLEV,GLARGS                                                    
         BL    EXIT                                                             
         MVI   TOTLINE,C'N'        SUPPRESS TOTAL ESTIMATES                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* HEADHOOK                                                                      
*                                                                               
DH500    MVC   H5(6),=C'CLIENT'  CLIENT HEADLINE                                
         CLI   QCLT,C'*'                                                        
         BNE   DH510                                                            
         MVC   H5+7(6),=C'OFFICE'                                               
         MVC   H5+14(1),QCLT+1                                                  
         B     EXIT                                                             
*                                                                               
DH510    MVC   H5+9(3),QCLT                                                     
         CLC   QCLT,=C'ALL'                                                     
         BE    EXIT                                                             
         MVC   H5+13(24),CLTNM                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* INPUT ROUTINES                                                                
*                                                                               
TOTESTI  MVC   0(4,R3),=F'1'                                                    
         B     EXIT                                                             
*                                                                               
ESTIM    ZIC   RE,BEST             GET ALPHA EST                                
         CVD   RE,DUB                                                           
         UNPK  0(3,R3),DUB                                                      
         OI    2(R3),X'F0'                                                      
         MVC   SVKEY,KEY           SAVE THE KEY                                 
         XC    KEY,KEY             GET ESTIMATE NAME                            
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),PRD                                                     
         MVC   KEY+7(1),BEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
         L     R6,ADEST                                                         
         MVC   4(20,R3),EDESC-ESTHDR(R6)                                        
         MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* OUTPUT ROUTINES                                                               
*                                                                               
TOTESTO  CLI   TOTLINE,C'Y'        ONLY PRINT FOR TOTAL LINE                    
         BNE   EXIT                                                             
         MVI   TOTLINE,C'N'                                                     
         MVI   GLHOOK,GLEDIT                                                    
         B     EXIT                                                             
         EJECT                                                                  
PUTDRIV  NTR1                                                                   
*                                                                               
*        ROUTINE TO PUT RECORDS TO DRIVER                                       
*                                                                               
         LA    RE,PROGPROF                                                      
         ST    RE,GLAPPROF                                                      
         MVI   GLMODE,GLINPUT                                                   
         MVI   ANYDATA,C'Y'        INDICATE THERE IS DATA                       
         GOTO1 ADRIVER,DMCB,(R7)                                                
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
AMGRTAB  DC    A(MGRTAB)                                                        
AESTTAB  DC    A(ESTTAB)                                                        
QMGRLEN  DS    H                                                                
*                                                                               
DPGFILE  DC    CL8'SPXL05  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
*                                                                               
XFF      DC    XL5'FFFFFFFFFF'                                                  
*                                                                               
DONESTS  DS    XL256                                                            
*                                                                               
QBMKT    DS    XL2                                                              
PRTSW    DS    CL1                                                              
TOTLINE  DS    CL1                                                              
ANYDATA  DS    CL1                                                              
MKTLEV   DS    CL1                                                              
FIRSTCLT DS    CL1                                                              
SVKEY    DS    CL32                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*ESTTAB*'                                                    
ESTTAB   DS    (256*256)X                                                       
*                                                                               
         DC    CL8'*MGRTAB*'                                                    
MGRTAB   DS    2500D                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'DRGLOBAL'                                                    
GLAREA   DS    40000X              DRIVER GLOBAL AREA                           
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
       ++INCLUDE SPDRVWRKD                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREPXL02 05/01/02'                                      
         END                                                                    
