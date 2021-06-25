*          DATA SET SPREPPE02  AT LEVEL 028 AS OF 05/01/02                      
*PHASE SPMC02A                                                                  
*INCLUDE COVAIL                                                                 
*INCLUDE MEDADDWT                                                               
         TITLE 'SPREPPE02 - POST BUY EXCEPTION REPORT'                          
*        PRINT NOGEN                                                            
SPMC02   CSECT                                                                  
         NMOD1 0,SPMC02,RA                                                      
*                                                                               
         L     R8,0(R1)                                                         
         LA    R9,2048(R8)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,R8,R9                                                    
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
*                                                                               
         L     RC,=A(GLAREA)                                                    
         USING GLOBALD,RC                                                       
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    SP200                                                            
         CLI   MODE,STALAST                                                     
         BE    SP300                                                            
         CLI   MODE,MKTFRST                                                     
         BE    SP160                                                            
         CLI   MODE,ESTFRST                                                     
         BE    SP150                                                            
         CLI   MODE,CLTFRST                                                     
         BE    SP130                                                            
         CLI   MODE,REQFRST                                                     
         BE    SP100                                                            
         CLI   MODE,REQLAST                                                     
         BE    SP600                                                            
         CLI   MODE,RUNFRST                                                     
         BE    SP010                                                            
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
LTXIT    CR    R8,R9                                                            
         B     EXIT                                                             
*                                                                               
GTXIT    CR    R9,R8                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* RUN FIRST                                                                     
*                                                                               
SP010    STM   R7,RC,BUYHKR7                                                    
*                                                                               
         MVC   MEDNUMPE,=F'1'      PERIOD ONLY                                  
         MVI   MEDEXTDM,4          4 DEMOS                                      
         MVC   MEDLCHNK,=AL4(MEDBY5-MEDGLD)                                     
         XC    MEDNUMWK,MEDNUMWK                                                
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVI   ALLOWCWM,C'N'     SO THAT MEDDATE WON'T CLOBBER MEDNOLNK         
         MVI   FIRSTSW,C'Y'      FIRST TIME SWITCH                              
*                                                                               
         XC    ADRIVER,ADRIVER                                                  
         XC    ASYSDRV,ASYSDRV                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* REQUEST FIRST                                                                 
*                                                                               
SP100    MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=X'0001'                                                    
         XC    HEADHOOK,HEADHOOK                                                
         MVI   ANYDATA,C'N'        NO DATA YET                                  
         LA    RE,GLOBALD          INIT DRIVER STORAGE TO ZEROS                 
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         MVC   GLSIZE,=F'40000'    GLOBAL SIZE                                  
         MVC   GLOBALD(8),=C'*GLOBAL*'                                          
*                                                                               
         OC    GLAPROG,GLAPROG     WAS DPG FILE LOADED PREVIOUSLY               
         BZ    SP105                                                            
         SR    R3,R3               YES - FREE STORAGE                           
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
         BNZ   SP120                                                            
         GOTO1 LOADER,DMCB,SPDRIVER     LOAD SYSTEM DRIVER                      
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ASYSDRV                                                       
*                                                                               
SP120    MVC   GLASYSDR,ASYSDRV    SYSTEM DRIVER ADDR                           
         LA    RE,SPWORKD                                                       
         ST    RE,GLAWORKD         SPOT WORK ADDR                               
         LA    RE,DRHOOK                                                        
         ST    RE,GLAHOOK          OUR DRIVER HOOK                              
         MVI   GLTWORKD,GLTSPOT    SPOT WORK                                    
         MVI   GLDETHED,C'Y'       I GET HEADS AT DETAIL TIME                   
         MVI   GLFHEADL,12         HEAD ROWS                                    
         MVI   GLLHEADL,13                                                      
*                                                                               
         MVI   PRTSW,C'Y'          INITIALIZE WORKING STORAGE                   
         MVI   CMLSW,0                                                          
         MVI   MGTOT,0                                                          
         XC    AMKTS,AMKTS                                                      
         XC    MKTCNTRS,MKTCNTRS                                                
         XC    RATING,RATING                                                    
         XC    RATINGEQ,RATINGEQ                                                
         XC    DMNAMES,DMNAMES                                                  
         XC    SVFILM,SVFILM                                                    
         XC    SVFLMNUM,SVFLMNUM                                                
         MVI   SVFLMNUM,X'FE'                                                   
         XC    SVMARKET,SVMARKET                                                
         XC    BUYKEYSV,BUYKEYSV                                                
         MVI   SPSUPMKT,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         L     RE,AMKTWTAB         CLEAR MARKET WEIGHT TABLE                    
         L     RF,AMKTWTBX                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         LA    R1,1                DETERMINE VALUE OF MEDGETBY 3RD PARM         
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R1,1(R1)            ADJUSTED                                     
         CLC   QBOOK1,SPACES                                                    
         BE    *+8                                                              
         LA    R1,5(R1)            IF RERATE, THEN RERATE BY AFFID              
         STC   R1,GETBYP2                                                       
*                                                                               
         CLI   QFILTER,C'F'        TEST FOR COMMERCIAL CLASS                    
         BE    *+8                                                              
         MVI   QOPT2,C'F'          NO - MAKE SURE WE LIST COMMERCIALS           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* CLIENT FIRST                                                                  
*                                                                               
SP130    MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         L     RE,GLAPLIST                                                      
         MVC   ASPDRWKC,0(RE)      A(SPOT DRIVER WORK AREA)                     
*                                                                               
         MVI   GLMODE,GLINPUT      SET UP FOR DRIVER INPUT                      
         LA    RE,PROGPROF                                                      
         ST    RE,GLAPPROF                                                      
*                                                                               
         CLI   FIRSTSW,C'Y'        TEST FIRST TIME                              
         BNE   SP131                                                            
         MVI   FIRSTSW,C'N'        YES - GET BUFFALO BUFFER                     
         L     R6,=A(BUFFALOC)                                                  
         GOTO1 COVAIL,DMCB,C'SETB',40000,1500000,(R6)                           
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFBUFF,DMCB+12                                                 
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF     INIT BUFFALO                   
*                                                                               
SP131    CLI   QFILTER,C'F'        TEST FOR FILM FILTER                         
         BE    *+12                                                             
         OI    CMLSW,FLM           NO - LIST COMMERCIALS                        
         B     SP132                                                            
         MVI   QFILTER,C' '        YES --                                       
         OI    CMLSW,CLS           LIST COMMERCIAL CLASS                        
         CLI   QOPT2,C'F'          TEST LIST COMMERCIALS AS WELL                
         BNE   SP132                                                            
         OI    CMLSW,FLM           YES                                          
*                                                                               
SP132    ZIC   RE,SDNPGRPS         DETERMINE LEVEL NUMBERS                      
         ZIC   RF,SDNMGRPS                                                      
         LA    RE,5(RE,RF)                                                      
         TM    CMLSW,FLM+CLS                                                    
         BNO   *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,STALEV           STATION LEVEL                                
         STC   RE,TOTLEV           DRIVER SHOULD GENERATE ALL TOTALS            
*                                  FOR TOTLEV AND ABOVE                         
*                                                                               
         LA    RE,COMLTAB          CLEAR COMMERCIAL TABLE                       
         L     RF,=A(COMLTABX)                                                  
         SR    RF,RE                                                            
         XCEF                                                                   
         LA    R2,COMLTAB          BUILD COMMERCIAL TABLE                       
         SR    R4,R4               ENTRY COUNTER                                
         L     R6,ADBUY            IO AREA                                      
         ST    R6,AREC                                                          
         USING CMLRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         GOTO1 HIGH                                                             
*                                                                               
SP133    CLC   KEY(5),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   SP136                                                            
         GOTO1 GET                                                              
         MVC   0(2,R2),CMLSEQ+1    FILM SEQUENCE NUMBER                         
         MVC   2(4,R2),CMLCLASS    CLASS                                        
         MVI   6(R2),X'FE'         INITIALIZE UNKNOWN CML NUMBER                
         MVC   14(15,R2),SPACES    INITIALIZE CML NAME                          
         OC    0(2,R2),0(R2)       TEST NULL FILM SEQ                           
         BZ    *+14                                                             
         OC    2(4,R2),2(R2)         OR MISSING CLASS                           
         BNZ   *+10                                                             
         MVC   2(4,R2),=C'ZZZZ'    YES - CLASS = ZZZZ                           
         CLI   QFILTER+1,C' '      TEST LIMIT TO ONE CLASS                      
         BE    *+14                NO                                           
         CLC   QFILTER+1(1),2(R2)  TEST INCLUDE THIS CLASS                      
         BNE   SP134               NO - SKIP                                    
         BCTR  R4,0                BUMP COUNTER                                 
         LA    R2,29(R2)           BUMP POINTER                                 
         C     R2,=A(COMLTABX)                                                  
         BL    SP134                                                            
         DC    H'0'                COMMERCIAL TABLE FULL                        
*                                                                               
SP134    GOTO1 SEQ                                                              
         B     SP133                                                            
         DROP  R6                                                               
*                                                                               
SP136    XC    CMLTBCNT,CMLTBCNT                                                
         LPR   R4,R4                                                            
         BZ    SP145                                                            
         ST    R4,CMLTBCNT                                                      
         GOTO1 XSORT,DMCB,COMLTAB,(R4),29,2,0    SORT ON FILM SEQUENCE          
*                                                                               
         LA    R2,COMLTAB          POST FILM NUMBERS AND NAMES TO TABLE         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AA1'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEYSAVE,KEY                                                      
*                                                                               
SP138    OC    0(2,R2),0(R2)                                                    
         BZ    SP140                                                            
         MVC   KEY+6(2),0(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SP140                                                            
         MVC   AREC,AWKAREA                                                     
         GOTO1 GET                                                              
         L     R3,AWKAREA                                                       
         USING CMLRECD,R3                                                       
         MVC   6(8,R2),CMLKCML     FILM NUMBER                                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   SP140                                                            
         USING CMLDTAEL,R3                                                      
         MVC   14(15,R2),CMLTITLE  FILM NAME                                    
         DROP  R3                                                               
*                                                                               
SP140    LA    R2,29(R2)                                                        
         MVC   KEY,KEYSAVE                                                      
         BCT   R4,SP138                                                         
*                                                                               
SP145    B     EXIT                                                             
         SPACE 2                                                                
         GETEL (R3),24,ELCODE                                                   
         EJECT                                                                  
*                                                                               
* ESTFRST                                                                       
*                                                                               
SP150    GOTO1 MEDDATE,DMCB,(R8)   BUILD MEDBLOCK                               
*                                                                               
         GOTO1 MEDPRDRD,DMCB,(R8)                                               
         LA    RE,220                                                           
         CLC   QPRD,=C'POL'                                                     
         BE    *+8                                                              
         IC    RE,BPRD                                                          
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     R3,PRDBUFF                                                       
         LA    R3,28(RE,R3)        POINT TO PRODUCT DEMO LIST                   
         ST    R3,ADEMLST          SAVE A(DEMO LIST)                            
*                                                                               
         CLC   QPRD,=C'POL'        ONLY GET DEMO NAMES IF THEY WON'T            
         BE    SP155               CHANGE                                       
         CLC   QEST(2),=C'NO'                                                   
         BE    SP155                                                            
         CLC   QESTEND,SPACES                                                   
         BNE   SP155                                                            
         B     EXIT                                                             
*                                                                               
SP155    OC    DMNAMES,DMNAMES     DO WE HAVE DEMO NAMES YET                    
         BNZ   EXIT                                                             
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBTPTT,C'T'                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         L     R4,ADEST                                                         
         USING ESTHDRD,R4                                                       
         LA    R4,EUSRNMS                                                       
         DROP  R4                  GET THE DEMO NAMES                           
         GOTO1 DEMOCON,DMCB,(4,(R3)),(2,DMNAMES),(C'S',DBLOCK),(R4)             
         DROP  R5                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* MKTFRST                                                                       
*                                                                               
SP160    MVI   MKTDATA,C'N'        NO MARKET ACTIVITY YET                       
         MVC   MKTSV,MKT                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PROCBUY                                                                       
*                                                                               
SP200    L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         XC    PSLIST,PSLIST                                                    
         GOTO1 MEDPSL,DMCB,(R8),PSLIST                                          
         LA    R2,PSLIST                                                        
*                                                                               
SP210    CLI   0(R2),0                                                          
         BE    SP250                                                            
         LA    RE,BUYHOOK          MEDGETBY HOOK                                
         ST    RE,SPOTHOOK                                                      
         MVI   QFILTER,C' '                                                     
         XC    CMLPTR,CMLPTR                                                    
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         CLI   MEDBRAND,X'FF'                                                   
         BNE   *+8                                                              
         MVI   MEDBRAND,219                                                     
         LA    RE,FILMLST          CLEAR FILM LIST                              
         LA    RF,FILMLSTL                                                      
         XCEF                                                                   
*                                                                               
SP211    MVC   DMCB+7(1),GETBYP2   DEMO TYPE PARM TO MEDGETBY                   
         GOTO1 MEDGETBY,DMCB,(R8)                                               
         CLI   MEDSPILL,C'Y'       CHECK FOR SPILL                              
         BNE   *+12                                                             
         CLI   SPOTPROF+5,0                                                     
         BE    SP240                                                            
*                                                                               
         CLI   QFILTER,C'F'                                                     
         BNE   SP212                                                            
         MVC   FILMSEQ,0(R3)                                                    
         TM    CMLSW,CLS                                                        
         BZ    SP214                                                            
         BAS   RE,LKUPFILM         GET FILM CLASS                               
         B     SP214                                                            
*                                                                               
SP212    OC    MEDBYSPT,MEDBYSPT   TEST ANY SPOTS WITH UNKNOWN CML              
         BZ    SP230                                   OR CLASS                 
         CLI   QOPT4,C'Y'          YES - TEST WANT DETAILS PRINTED              
         BNE   SP213                                                            
         CLC   BUYKEY(11),BUYKEYSV YES - TEST PRINTED THIS BUYLINE YET          
         BE    SP213                                                            
         MVC   BUYKEYSV,BUYKEY           NO - PRINT                             
         LA    R5,P                                                             
         USING PLINED,R5                                                        
         MVC   PCLT,CLT                                                         
         MVC   PPRD,PRD                                                         
         MVC   PMKT,MKT                                                         
         MVC   PSTA,STAPRINT                                                    
         EDIT  (1,BUYKEST),(3,PEST)                                             
         EDIT  (1,BUYKBUY),(3,PLN)                                              
         GOTO1 REPORT                                                           
         DROP  R5                                                               
*                                                                               
SP213    XC    FILMSEQ,FILMSEQ     UNKNOWN CML/CLASS                            
         MVC   CLASS,=C'ZZZZ'                                                   
*                                                                               
SP214    XC    BFREC,BFREC         CLEAR BUFFALO REC                            
         TM    CMLSW,FLM                                                        
         BZ    *+10                                                             
         MVC   BFFILM,FILMSEQ      COMMERCIAL                                   
         TM    CMLSW,CLS                                                        
         BZ    *+10                                                             
         MVC   BFCLASS,CLASS       COMMERCIAL CLASS                             
         MVC   BFPRD,MEDBRAND      PRODUCT                                      
         MVC   BFSTA,BSTA          STATION                                      
         MVC   BFSPT,MEDBYSPT      SPOTS                                        
         MVC   BFDOL,MEDBYD        BUY DOLLARS                                  
         MVC   BFDOLEQ,MEDBYDEQ    EQUIV DOLLARS                                
         MVC   BFDEM1(32),MEDBY1   4 X DEMO/DEMEQUIV                            
         MVI   BFMAXTLV,0                                                       
*                                                                               
SP215    MVC   BFDPTGRP,MEDDPGNO   DAYPART GROUP                                
         MVC   BFDPT,MEDDPNO       DAYPART                                      
         MVC   BFLEN,MEDSPTLN      LENGTH                                       
         BAS   RE,PUTBUFF          PUT TO BUFFALO                               
         MVC   BFMAXTLV,TOTLEV                                                  
         MVI   BFLEN,X'FF'         DAYPART TOTAL                                
         BAS   RE,PUTBUFF                                                       
         MVC   BFDPT,XFF                                                        
         OC    BFDPTGRP,BFDPTGRP   TEST FOR DAYPART GROUP                       
         BZ    *+8                                                              
         BAS   RE,PUTBUFF          YES - PUT GROUP TOTAL                        
         CLI   PROGPROF+2,C'Y'     TEST FOR CROSS DAYPARTS SUPPRESSED           
         BE    SP220                                                            
         MVC   BFDPTGRP,XFF                                                     
         CLI   PROGPROF+1,C'Y'     TEST FOR SPOT LENGTH TOTALS                  
         BNE   *+18                                                             
         MVC   BFLEN,MEDSPTLN      YES                                          
         BAS   RE,PUTBUFF                                                       
         MVI   BFLEN,X'FF'                                                      
         BAS   RE,PUTBUFF          STATION TOTAL                                
*                                                                               
SP220    CLC   BFSTA,XFF           TEST FOR MARKET TOTAL DONE                   
         BE    SP230                                                            
         MVC   BFSTA,XFF                                                        
         MVC   BFMAXTLV,TOTLEV                                                  
         B     SP215                                                            
*                                                                               
SP230    CLI   QFILTER,C'F'                                                     
         BE    SP235                                                            
         MVI   QFILTER,C'F'                                                     
         XC    SPOTHOOK,SPOTHOOK                                                
         LA    R3,FILMLST                                                       
         MVC   CMLPTR,=A(CTAB)                                                  
         XC    CTAB,CTAB                                                        
         MVC   CTAB(4),=C'AAAA'                                                 
         B     SP237                                                            
*                                                                               
SP235    LA    R3,2(R3)                                                         
         C     R3,=A(FILMLSTX)                                                  
         BNL   SP240                                                            
*                                                                               
SP237    OC    0(2,R3),0(R3)                                                    
         BZ    SP240                                                            
         MVC   CTAB+4(2),0(R3)                                                  
         B     SP211                                                            
*                                                                               
SP240    LA    R2,2(R2)            NEXT IN PRODUCT/LENGTH LIST                  
         B     SP210                                                            
*                                                                               
SP250    MVI   QFILTER,C' '                                                     
         XC    CMLPTR,CMLPTR                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* MEDGETBY HOOK                                                                 
*                                                                               
         USING *,RF                                                             
BUYHOOK  NTR1                                                                   
         LM    R7,RC,BUYHKR7                                                    
         B     BH010                                                            
BUYHKR7  DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
BH010    L     R6,SPOTADDR                                                      
         SR    R0,R0                                                            
         MVI   SECBRAND,C'N'                                                    
         CLI   0(R6),X'0B'         TEST PROCESSING SECOND BRAND                 
         BL    BH020                                                            
         CLI   0(R6),X'0D'                                                      
         BH    BH020                                                            
         CLI   1(R6),18                                                         
         BNE   BH020                                                            
         CLC   MEDBRAND,14(R6)                                                  
         BNE   BH020                                                            
         MVI   SECBRAND,C'Y'                                                    
*                                                                               
BH020    ICM   R0,1,1(R6)          LOOK FOR FILM ELEMENT                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),X'12'                                                      
         BE    BH030                                                            
         CLI   0(R6),0                                                          
         BE    BH900               NO FILM ELE                                  
         CLI   0(R6),6                                                          
         BL    BH020                                                            
         CLI   0(R6),8                                                          
         BNH   BH900               NO FILM ELE                                  
         CLI   0(R6),X'0B'                                                      
         BL    BH020                                                            
         CLI   0(R6),X'0D'                                                      
         BNH   BH900               NO FILM ELE                                  
         B     BH020                                                            
*                                                                               
         USING FLMELEM,R6          FILM ELE FOUND                               
BH030    LA    R4,FLMNUM           R4 = A(FILM NUMBER)                          
         CLI   FLMLEN,5            TEST SECOND FILM                             
         BNH   BH035                                                            
         CLI   SECBRAND,C'Y'       YES - TEST PROCESSING SECOND BRAND           
         BNE   BH035                                                            
         LA    R4,FLMNUM+2         YES - USE SECOND FILM                        
*                                                                               
BH035    OC    0(2,R4),0(R4)       TEST NULL FILM                               
         BZ    BH900                                                            
         MVC   FILMSEQ,0(R4)                                                    
         BAS   RE,LKUPFILM                                                      
         CLI   FILMNUM,X'FE'       TEST UNKNOWN FILM                            
         BE    BH900                                                            
         TM    CMLSW,FLM           TEST CLASS ONLY AND CLASS UNKNOWN            
         BO    BH040                                                            
         CLC   CLASS,=C'ZZZZ'                                                   
         BE    BH900                                                            
*                                                                               
BH040    LA    R3,FILMLST          ADD FILM TO FILM LIST                        
*                                                                               
BH050    OC    0(2,R3),0(R3)                                                    
         BNZ   *+14                                                             
         MVC   0(2,R3),0(R4)                                                    
         B     BH990                                                            
         CLC   0(2,R3),0(R4)                                                    
         BE    BH990                                                            
         LA    R3,2(R3)                                                         
         C     R3,=A(FILMLSTX)                                                  
         BL    BH050                                                            
         DC    H'0'                                                             
*                                                                               
BH900    TM    CMLSW,CLS           NO FILM ELE, UNKNOWN FILM OR CLASS           
         BZ    BH999               IF NOT CML CLASS, EXIT(FILM UNKNOWN)         
         CLI   QFILTER+1,C' '                                                   
         BE    BH999                                                            
         CLI   QFILTER+1,C'Z'                                                   
         BE    BH999                                                            
*                                                                               
BH990    MVI   SPOTYORN,C'N'                                                    
         B     EXIT                                                             
*                                                                               
BH999    MVI   SPOTYORN,C'Y'       PASS ONLY UNKNOWN FILM                       
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* STALAST                                                                       
*                                                                               
SP300    BAS   RE,PUTDRIV          PUT REMAINING BUFFALO RECS TO DRIVER         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* REQUEST LAST                                                                  
*                                                                               
SP600    CLI   ANYDATA,C'Y'        TEST FOR ANY DATA                            
         BNE   EXIT                                                             
         MVI   GLMODE,GLOUTPUT     DRIVER OUTPUT PHASE                          
         MVI   MODE,REQFRST        FUDGE THE MODE TO PRINT HEADLINES            
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    EBLOCK,EBLOCK       SET UP EDITOR FOR EDITING CPP                
         LA    R1,FULL                                                          
         ST    R1,EBAIN                                                         
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBLOUT,7                                                         
         MVI   EBDECS,2                                                         
*                                                                               
         ICM   R4,15,CMLTBCNT      RE-SORT CML TABLE IN CML NUMBER SEQ          
         BZ    SP610                                                            
         GOTO1 XSORT,DMCB,COMLTAB,(R4),29,8,6                                   
*                                                                               
SP610    XC    SVFLMNUM,SVFLMNUM                                                
         MVI   SVFLMNUM,X'FE'                                                   
         GOTO1 ADRIVER,DMCB,(RC)                                                
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
         CLI   GLHOOK,GLRESLIT     TEST TO RESOLVE LITERAL                      
         BE    DH300                                                            
         CLI   GLHOOK,GLFIRST      TEST FOR FIRST TIME CONTROL                  
         BE    DH400                                                            
         CLI   GLHOOK,GLHEAD       TEST FOR HEADLINE HOOK                       
         BE    DH600                                                            
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
         DC    C'PRODUCT ',A(PROD)                                              
         DC    C'MKTGRP1I',A(MKTGRP1I)                                          
         DC    C'MKTGRP2I',A(MKTGRP2I)                                          
         DC    C'MKTGRP3I',A(MKTGRP3I)                                          
         DC    C'CLASSI  ',A(CLASSI)                                            
         DC    C'CLASSO  ',A(CLASSO)                                            
         DC    C'FILMI   ',A(FILMI)                                             
         DC    C'FILMO   ',A(FILMO)                                             
         DC    C'MARKETO ',A(MKTO)                                              
         DC    C'STATIONI',A(STATIONI)                                          
         DC    C'STATIONO',A(STATIONO)                                          
         DC    C'DPTLENI ',A(DPTLENI)                                           
         DC    C'DPTLENO ',A(DPTLENO)                                           
         DC    C'SPOTS   ',A(SPOT)                                              
         DC    C'BUYD    ',A(BUYD)                                              
         DC    C'BUYDO   ',A(BUYDO)                                             
         DC    C'BUYDEQ  ',A(BUYDEQ)                                            
         DC    C'BUYDEQO ',A(BUYDEQO)                                           
         DC    C'BUYDEMO ',A(BUYDEMO)                                           
         DC    C'BUYDMOUT',A(BUYDMOUT)                                          
         DC    C'BUYDEMEQ',A(BUYDEMEQ)                                          
         DC    C'BUYDMEQO',A(BUYDMEQO)                                          
         DC    C'CPP     ',A(CPP)                                               
         DC    C'DEMHED  ',A(DEMHED)                                            
         DC    C'PRDFRST ',A(PRDFST)                                            
         DC    C'ESTFRST ',A(ESTFST)                                            
         DC    C'MGRPFRST',A(MGRPFST)                                           
         DC    C'MKTFRST ',A(MKTFST)                                            
         DC    C'FILMFRST',A(FILMFST)                                           
         DC    C'CLSSFRST',A(CLASSFST)                                          
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
* INPUT ROUTINES                                                                
*                                                                               
PROD     MVC   0(3,R3),=C'POL'     PRODUCT                                      
         CLI   BFPRD,219                                                        
         BNL   EXIT                                                             
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
PROD2    CLC   BFPRD,3(RF)                                                      
         BE    PRODX                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNL   PROD2                                                            
         DC    H'0'                                                             
*                                                                               
PRODX    MVC   0(3,R3),0(RF)                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
         USING SPDRVWKD,R6                                                      
MKTGRP1I L     R6,ASPDRWKC             MARKET GROUP 1                           
         CLI   MGTOT,0                                                          
         BE    *+22                                                             
         CLI   MGTOT,1                                                          
         BH    *+14                                                             
         MVC   0(2,R3),XFF                                                      
         B     EXIT                                                             
         MVC   0(2,R3),BMGR+1                                                   
         OC    0(2,R3),MGR1MASK                                                 
         B     EXIT                                                             
*                                                                               
MKTGRP2I L     R6,ASPDRWKC             MARKET GROUP 2                           
         CLI   MGTOT,0                                                          
         BE    *+22                                                             
         CLI   MGTOT,2                                                          
         BH    *+14                                                             
         MVC   0(2,R3),XFF                                                      
         B     EXIT                                                             
         MVC   0(2,R3),BMGR+1                                                   
         OC    0(2,R3),MGR2MASK                                                 
         B     EXIT                                                             
*                                                                               
MKTGRP3I CLI   MGTOT,0                 MARKET GROUP 3                           
         BE    *+14                                                             
         MVC   0(2,R3),XFF                                                      
         B     EXIT                                                             
         MVC   0(2,R3),BMGR+1                                                   
         B     EXIT                                                             
*                                                                               
*                                                                               
CLASSI   MVC   0(4,R3),BFCLASS     COMMERCIAL CLASS                             
         B     EXIT                                                             
*                                                                               
FILMI    CLC   SVFILM,BFFILM       COMMERCIAL                                   
         BE    FILMI2                                                           
         MVC   SVFILM,BFFILM                                                    
         CLC   SVFILM,XFF                                                       
         BNE   *+14                                                             
         MVC   SVFLMNUM,XFF                                                     
         B     FILMI2                                                           
         MVC   FILMSEQ,BFFILM                                                   
         BAS   RE,LKUPFILM                                                      
         MVC   SVFLMNUM,FILMNUM                                                 
FILMI2   MVC   0(8,R3),SVFLMNUM                                                 
         B     EXIT                                                             
*                                                                               
STATIONI CLC   BFSTA,XFF           STATION                                      
         BNE   *+14                                                             
         MVC   0(5,R3),XFF                                                      
         B     EXIT                                                             
         MVC   WORK+2(3),BFSTA                                                  
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,0(R3)                                    
         B     EXIT                                                             
*                                                                               
DPTLENI  MVC   0(9,R3),BFDPTLEN    DAYPART/LENGTH                               
         B     EXIT                                                             
*                                                                               
SPOT     MVC   0(4,R3),BFSPT       SPOTS                                        
         B     EXIT                                                             
*                                                                               
BUYD     L     RE,BFDOL            DOLLARS                                      
         B     CVD                                                              
*                                                                               
BUYDEQ   L     RE,BFDOLEQ          EQUIV DOLLARS                                
         B     CVD                                                              
*                                                                               
BUYDEMO  LA    RE,BFDEM1           DEMO                                         
         B     BUYDEM2                                                          
*                                                                               
BUYDEMEQ LA    RE,BFDEM1EQ         EQUIV DEMO                                   
*                                                                               
BUYDEM2  ZIC   RF,GLARGS                                                        
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         AR    RE,RF                                                            
         MVC   0(4,R3),0(RE)                                                    
         B     EXIT                                                             
*                                                                               
CVD      CVD   RE,0(R3)                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* OUTPUT ROUTINES                                                               
*                                                                               
CLASSO   MVC   0(45,R3),SPACES     COMMERCIAL CLASS                             
         CLC   0(4,R2),XFF                                                      
         BE    EXIT                                                             
         MVC   0(3,R3),ASTS                                                     
         MVC   4(16,R3),=C'COMMERCIAL CLASS'                                    
         MVC   21(4,R3),0(R2)                                                   
         MVC   26(3,R3),ASTS                                                    
         GOTO1 SQUASHER,DMCB,(R3),45                                            
         B     EXIT                                                             
*                                                                               
FILMO    MVC   0(45,R3),SPACES     COMMERCIAL                                   
         CLC   0(8,R2),XFF                                                      
         BE    EXIT                                                             
         MVC   0(3,R3),ASTS                                                     
         MVC   4(10,R3),=C'COMMERCIAL'                                          
         CLI   0(R2),X'FE'                                                      
         BE    FILMO3                                                           
         MVC   15(8,R3),0(R2)                                                   
         CLC   SVFLMNUM,0(R2)                                                   
         BE    FILMO2                                                           
         MVC   SVFLMNUM,0(R2)                                                   
         XC    WORK,WORK                                                        
         MVC   WORK+6(8),0(R2)                                                  
         LA    R5,COMLTAB                                                       
         ICM   R6,15,CMLTBCNT                                                   
         BZ    FILMO3                                                           
         GOTO1 BINSRCH,DMCB,(0,WORK),(R5),(R6),29,(6,8),(R6)                    
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         MVC   FILMNAME,14(RF)                                                  
FILMO2   MVI   24(R3),C'-'                                                      
         MVC   26(15,R3),FILMNAME                                               
         B     FILMO4                                                           
*                                                                               
FILMO3   MVC   15(8,R3),=C'UNKNOWN '                                            
*                                                                               
FILMO4   MVC   42(3,R3),ASTS                                                    
         GOTO1 SQUASHER,DMCB,(R3),45                                            
         B     EXIT                                                             
*                                                                               
MKTO     CLC   SVMARKET,0(R2)      MARKET OUTPUT                                
         BE    MKTO2                                                            
         MVC   SVMARKET,0(R2)      NEW MARKET - GET THE MARKET RECORD           
         CLC   SVMARKET,XFF                                                     
         BE    MKTO2                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),0(R2)                                                   
         MVC   KEY+6(2),QAGY                                                    
         MVI   KEY+8,C'0'                                                       
         MVC   KEY+9(8),KEY+8                                                   
         GOTO1 READMKT                                                          
*                                                                               
MKTO2    CLC   SVMARKET,XFF                                                     
         BNE   *+14                                                             
         MVC   0(5,R3),=C'*ALL*'                                                
         B     EXIT                                                             
         CLI   QOPT3,C'S'          TEST SUMMARY ONLY                            
         BNE   *+12                                                             
         MVI   PRTSW,C'N'          YES - SUPPRESS PRINT                         
         B     EXIT                                                             
         MVC   0(4,R3),MKT                                                      
         MVC   5(19,R3),MKTNM                                                   
         B     EXIT                                                             
*                                                                               
STATIONO CLC   0(3,R2),XFF         STATION                                      
         BNE   STATO2                                                           
         CLI   QOPT3,C'S'          MARKET TOTAL - TEST SUMMARY ONLY             
         BE    STATO1              YES                                          
         CP    STATCNTR,=P'2'      NO - TEST MORE THAN 1 STATION                
         BH    STATO1                                                           
         MVI   PRTSW,C'N'               NO - SUPPRESS TOTAL                     
         B     EXIT                                                             
STATO1   MVC   0(5,R3),=C'*ALL*'                                                
         B     EXIT                                                             
*                                                                               
STATO2   MVC   0(4,R3),0(R2)                                                    
         CLI   4(R2),C' '                                                       
         BE    EXIT                                                             
         CLI   3(R3),C' '                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         MVI   4(R3),C'-'                                                       
         MVC   5(1,R3),4(R2)                                                    
         MVI   6(R3),C'M'                                                       
         B     EXIT                                                             
*                                                                               
BUYDO    MVC   DOL,0(R2)           SAVE COST                                    
         MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     EXIT                                                             
*                                                                               
BUYDEQO  MVC   DOLEQ,0(R2)         SAVE EQUIV COST                              
         B     EXIT                                                             
*                                                                               
*                                                                               
BUYDMOUT MVC   RATING,0(R2)        SAVE RATING                                  
         LA    R1,RATING                                                        
         BAS   RE,BUYDWGT                                                       
         CLC   RATING,=F'1000000'  TEST RATING IS HIGH                          
         BNL   *+12                                                             
         MVI   GLHOOK,GLEDIT                                                    
         B     EXIT                                                             
         MVC   FULL,RATING         YES - EDIT NOW                               
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         ST    R3,EBAOUT                                                        
         L     RF,GLAEDITR                                                      
         GOTO1 (RF),DMCB,EBLOCK                                                 
         MVI   EBDECS,2                                                         
         MVI   EBSCIN,0                                                         
         B     EXIT                                                             
*                                                                               
BUYDMEQO MVC   RATINGEQ,0(R2)      SAVE EQUIV RATING                            
         LA    R1,RATINGEQ                                                      
         BAS   RE,BUYDWGT                                                       
         B     EXIT                                                             
*                                                                               
BUYDWGT  LR    R0,RE                                                            
         CLI   CROSSMKT,C'Y'       TEST FOR ALL MARKETS                         
         BNE   BUYDWGT2                                                         
         ZIC   RE,GLARGS           YES - TEST WHETHER THIS DEMO IS              
         BCTR  RE,0                      WEIGHTED                               
         MH    RE,=H'7'                                                         
         LA    RE,DMNAMES(RE)                                                   
         CLI   0(RE),C'R'                                                       
         BE    *+12                                                             
         CLI   SPOTPROF+1,C'D'                                                  
         BNE   BUYDWGT2                                                         
         BAS   RE,UNWEIGHT               YES - UNWEIGHT THE DEMO                
BUYDWGT2 LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
UNWEIGHT SR    R5,R5                                                            
         ICM   RF,15,TOTWGT        TOTAL WEIGHT                                 
         BZ    UNWT2                                                            
         ICM   R5,15,0(R1)         DEMO VALUE                                   
         BZ    UNWT2                                                            
         SR    R4,R4                                                            
         AR    R5,R5               X2                                           
         DR    R4,RF               DIVIDE BY TOTAL WEIGHT                       
         SRA   R5,1                ROUND                                        
UNWT2    ST    R5,0(R1)                                                         
         ST    R5,0(R2)                                                         
         BR    RE                                                               
*                                                                               
*                                                                               
CPP      CLI   CROSSDPT,C'Y'       TEST FOR CROSS DAYPARTS NOW                  
         BNE   *+12                                                             
         CLI   PROGPROF,C'Y'       YES - TEST FOR WHETHER TO PRINT              
         BNE   CPPX                      NO - EXIT                              
         MVI   EQUIV,C'N'                                                       
         SR    R0,R0                                                            
         CVB   R1,DOL                                                           
         L     RF,RATING                                                        
         CLI   SPOTPROF+4,C'Y'     TEST FOR EQUIV THE DETAIL LINES              
         BE    *+12                                                             
         CLI   DPTOT,C'Y'          NO - IS THIS A TOTAL LINE                    
         BNE   CPP2                     NO - NO EQUIVALENCING                   
         MVI   EQUIV,C'Y'                                                       
         CLI   SPOTPROF,C'D'       EQUIVALENCE BY DOLLARS OR RATING             
         BNE   *+12                                                             
         CVB   R1,DOLEQ                                                         
         B     CPP2                                                             
         L     RF,RATINGEQ                                                      
*                                                                               
CPP2     LTR   RF,RF               TEST FOR ZERO RATING                         
         BZ    CPPX                                                             
         M     R0,=F'10'           X 10 FOR 2 DECIMAL POINT PRECISION           
         DR    R0,RF               DIVIDE TO GET CPP                            
         LTR   R1,R1               TEST FOR ZERO CPP                            
         BZ    CPPX                                                             
         ST    R1,FULL                                                          
         C     R1,=F'1000000'                                                   
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         ST    R3,EBAOUT                                                        
         L     RF,GLAEDITR                                                      
         GOTO1 (RF),DMCB,EBLOCK                                                 
         MVI   EBDECS,2                                                         
         MVI   EBSCIN,0                                                         
         XC    RATING,RATING                                                    
         XC    RATINGEQ,RATINGEQ                                                
         CLI   EQUIV,C'Y'          TEST FOR EQUIVALENCED                        
         BNE   CPPX                                                             
         MVI   7(R3),C'+'          YES - MARK THE CPP                           
*                                                                               
CPPX     XC    RATING,RATING                                                    
         XC    RATINGEQ,RATINGEQ                                                
         B     EXIT                                                             
         EJECT                                                                  
DPTLENO  XC    DOL,DOL             CLEAR THE COSTS                              
         XC    DOLEQ,DOLEQ                                                      
         MVI   DPTOT,C'Y'          DAYPART TOTAL                                
         MVI   CROSSDPT,C'Y'       CROSS DAYPART TOTAL                          
         CLC   0(9,R2),XFF                                                      
         BNE   DL010                                                            
         CP    DLCNTR,=P'1'        TEST FOR MORE THAN ONE DETAIL                
         BNH   DL900                                                            
         CLI   PROGPROF+1,C'Y'     TEST FOR SPOT LENGTH TOTALS                  
         BNE   *+12                                                             
         CLI   SLCNTRS+3,0         YES - TEST FOR MORE THAN 1 SPTLEN            
         BE    DL900                     NO - DON'T PRINT TOTAL                 
         MVC   1(9,R3),=C'* TOTAL *'                                            
         B     EXIT                                                             
*                                                                               
DL010    CLC   0(8,R2),XFF         SPOT LENGTH TOTAL                            
         BNE   DL040                                                            
         LA    R0,5                                                             
         LA    R1,SLCNTRS                                                       
*                                                                               
DL020    CLC   0(1,R1),8(R2)                                                    
         BE    DL030                                                            
         LA    R1,3(R1)                                                         
         BCT   R0,DL020                                                         
         DC    H'0'                                                             
*                                                                               
DL030    CP    1(2,R1),=P'1'                                                    
         BNH   DL900                                                            
         MVC   1(6,R3),=C'TOTAL-'                                               
         B     DL100                                                            
*                                                                               
DL040    MVI   CROSSDPT,C'N'       NOT CROSS DPT TOTAL                          
         CLC   4(5,R2),XFF         DAYPART GROUP TOTAL                          
         BNE   DL050                                                            
         CP    DPTCNTR,=P'1'       TEST FOR MORE THAN ONE DPT IN GRP            
         BNH   DL900                                                            
         MVC   0(3,R3),1(R2)                                                    
         MVI   3(R3),C'-'                                                       
         MVC   4(6,R3),=C'TOTAL*'                                               
         B     EXIT                                                             
*                                                                               
DL050    CLI   8(R2),X'FF'         TEST FOR DAYPART TOTAL                       
         BE    DL095                                                            
         MVI   DPTOT,C'N'          NOT DAYPART TOTAL                            
         AP    DLCNTR,=P'1'        AUGMENT DPTLEN DETAIL COUNTER                
         LA    R0,5                AUGMENT APPROPRIATE SPOT LENGTH CNTR         
         LA    R1,SLCNTRS                                                       
*                                                                               
DL060    CLI   0(R1),0                                                          
         BE    DL070                                                            
         CLC   0(1,R1),8(R2)                                                    
         BE    DL080                                                            
         LA    R1,3(R1)                                                         
         BCT   R0,DL060                                                         
         DC    H'0'                                                             
*                                                                               
DL070    MVC   0(1,R1),8(R2)                                                    
         ZAP   1(2,R1),=P'0'                                                    
*                                                                               
DL080    AP    1(2,R1),=P'1'                                                    
         MVI   7(R3),C'-'                                                       
         AP    LENCNTR,=P'1'       INCREMENT LENGTH COUNTER                     
         CP    LENCNTR,=P'1'       IS IT GREATER THAN 1                         
         BH    DL100                                                            
         OC    0(4,R2),0(R2)       NO - FORMAT THE DAYPART                      
         BZ    DL090                                                            
         CLC   DPTGRPSV,0(R2)      TEST FOR NEW DAYPART GROUP                   
         BE    DL090                                                            
         ZAP   DPTCNTR,=P'0'       RESET DPT COUNTER                            
         MVC   DPTGRPSV,0(R2)                                                   
         CLC   1(3,R2),5(R2)       TEST DAYPART GROUP = DAYPART                 
         BE    DL090               YES - DON'T PRINT DAYPART GROUP              
         MVC   0(3,R3),1(R2)                                                    
         MVI   3(R3),C'-'                                                       
*                                                                               
DL090    MVC   4(3,R3),5(R2)                                                    
         AP    DPTCNTR,=P'1'       AUGMENT DAYPART COUNTER                      
         B     DL100                                                            
*                                                                               
DL095    CP    LENCNTR,=P'1'       DAYPART TOTAL                                
         BNH   *+14                - ONLY PRINT IF MORE THAN 1 LENGTH           
         MVC   7(3,R3),=C'TOT'                                                  
         B     *+8                                                              
         MVI   PRTSW,C'N'                                                       
         ZAP   LENCNTR,=P'0'       INIT LENGTH COUNTER                          
         B     EXIT                                                             
*                                                                               
DL100    EDIT  (1,8(R2)),(2,8(R3))    EDIT THE SPOT LENGTH                      
         B     EXIT                                                             
*                                                                               
DL900    MVI   PRTSW,C'N'          DON'T PRINT                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* HEADLINE ROUTINES                                                             
*                                                                               
DEMHED   MVC   CPPTITL,SPACES                                                   
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         MH    RE,=H'7'                                                         
         LA    RF,DMNAMES(RE)                                                   
         CLC   0(7,RF),SPACES                                                   
         BNH   EXIT                                                             
         MVC   0(7,R3),0(RF)                                                    
         MVC   CPPTITL,=C'CPP'                                                  
         CLI   0(R3),C'R'                                                       
         BE    EXIT                                                             
         MVC   CPPTITL,=C'CPM'                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* FIRSTS - PRODUCT FIRST                                                        
*                                                                               
PRDFST   MVC   PRD,0(R2)           SAVE THE PRODUCT CODE                        
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
PRDFST2  CLC   PRD,0(RF)                                                        
         BE    PRDFST4                                                          
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    PRDFST2                                                          
         DC    H'0'                                                             
*                                                                               
PRDFST4  MVC   BPRD,3(RF)                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ESTIMATE FIRST                                                                
*                                                                               
ESTFST   CLC   QPRD,=C'POL'        ONLY GET DEMO NAMES IF THEY MIGHT            
         BE    ESTFST2             CHANGE                                       
         CLC   QEST(2),=C'NO'                                                   
         BE    ESTFST2                                                          
         CLC   QESTEND,SPACES                                                   
         BNE   ESTFST2                                                          
         XC    KEY,KEY             READ THE ESTIMATE HEADER                     
         MVC   KEY+1(3),BAGYMD                                                  
         MVC   KEY+4(3),PRD                                                     
         PACK  DUB,0(3,R2)         ESTIMATE NUM                                 
         CVB   RE,DUB                                                           
         STC   RE,KEY+7                                                         
         STC   RE,BEST                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
*                                                                               
         GOTO1 MEDPRDRD,DMCB,(R8)                                               
*                                                                               
         ZIC   RE,BPRD                                                          
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     R3,PRDBUFF                                                       
         LA    R3,28(RE,R3)        POINT TO PRODUCT DEMO NUMBERS                
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBTPTT,C'T'                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         L     R4,ADEST                                                         
         USING ESTHDRD,R4                                                       
         LA    R4,EUSRNMS                                                       
         DROP  R4                  GET THE DEMO NAMES                           
         GOTO1 DEMOCON,DMCB,(4,(R3)),(2,DMNAMES),(C'S',DBLOCK),(R4)             
         DROP  R5                                                               
*                                                                               
ESTFST2  L     RE,AMKTWTAB         TURN OFF ALL MARKET ENCOUNTERED              
*                                  SWITCHES                                     
ESTFST4  OC    0(8,RE),0(RE)                                                    
         BZ    EXIT                                                             
         NI    4(RE),FF-MESTIM                                                  
         LA    RE,8(RE)                                                         
         B     ESTFST4                                                          
         EJECT                                                                  
*                                                                               
* COMMERCIAL CLASS FIRST                                                        
*                                                                               
CLASSFST CLC   0(4,R2),XFF         TEST FOR ALL COMMERCIAL CLASSES              
         BNE   CLSFST2                                                          
         MVI   MODE,ESTLAST        SUPPRESS MKTGRP HEADLINES                    
         MVC   AMKTS,=A(MKTALL)                                                 
         B     EXIT                                                             
*                                                                               
CLSFST2  L     RE,AMKTWTAB         TURN OFF MARKET ENCOUNTERED SWITCHES         
CLSFST4  OC    0(8,RE),0(RE)                                                    
         BZ    EXIT                                                             
         NI    4(RE),FF-MCLASS                                                  
         LA    RE,8(RE)                                                         
         B     CLSFST4                                                          
         SPACE 2                                                                
*                                                                               
* COMMERCIAL FIRST                                                              
*                                                                               
FILMFST  CLC   0(8,R2),XFF         TEST FOR ALL COMMERCIALS                     
         BNE   EXIT                                                             
         OC    AMKTS,AMKTS         IF HIGHER LEVEL TOTAL, EXIT                  
         BNZ   EXIT                                                             
         MVI   MODE,ESTLAST        SUPPRESS MKTGRP HEADLINES                    
         MVC   AMKTS,=A(MKTCLS)                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* MARKET GROUP FIRST                                                            
*                                                                               
MGRPFST  CLC   0(2,R2),XFF         TEST FOR ALL AT THIS MGRP LEVEL              
         BNE   EXIT                                                             
         OC    AMKTS,AMKTS         IF HIGHER LEVEL TOTAL, EXIT                  
         BNZ   EXIT                                                             
         CLI   GLARGS,1            MKTGRP1                                      
         BNE   *+14                                                             
         MVI   MODE,ESTLAST        SUPPRESS MKTGRP HEADLINES                    
         MVC   AMKTS,=A(MKTFLM)                                                 
         CLI   GLARGS,2            MKTGRP2                                      
         BNE   *+14                                                             
         MVI   MODE,MGR1LAST       SUPPRESS MKTGRP2/3 HEADLINES                 
         MVC   AMKTS,=A(MKTMG1)                                                 
         CLI   GLARGS,3            MKTGRP3                                      
         BNE   *+14                                                             
         MVI   MODE,MGR2LAST       SUPPRESS MKTGRP3 HEADLINES                   
         MVC   AMKTS,=A(MKTMG2)                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* MARKET FIRST                                                                  
*                                                                               
MKTFST   CLC   0(4,R2),XFF         TEST FOR ALL MARKETS                         
         BNE   MKTFST2                                                          
         MVI   FORCEHED,C'Y'       YES - FORCE NEW PAGE                         
         MVI   CROSSMKT,C'Y'                                                    
         ZAP   STATCNTR,=P'9'            MAKE SURE LINE WILL PRINT              
         OC    AMKTS,AMKTS               SET MARKET TOTAL ADDRESS               
         BNZ   *+10                                                             
         MVC   AMKTS,=A(MKTMG3)                                                 
         L     RE,AMKTS                  GET THE TOTAL MARKET WEIGHT            
         MVC   TOTWGT,4(RE)                                                     
         B     EXIT                                                             
*                                                                               
MKTFST2  MVI   MODE,REQFRST        NO - FORCE ALL HEADLINES TO PRINT            
         ZAP   STATCNTR,=P'0'           ZAP THE STATION COUNTER                 
         MVI   CROSSMKT,C'N'            NOT CROSS MARKETS                       
         XC    TOTWGT,TOTWGT            NO TOTAL WEIGHT                         
         L     R4,AMKTWTAB              FIND THE MARKET WEIGHT                  
*                                                                               
MKTFST4  OC    0(8,R4),0(R4)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   0(4,R4),0(R2)                                                    
         BE    *+12                                                             
         LA    R4,8(R4)                                                         
         B     MKTFST4                                                          
         SR    RF,RF                                                            
         ICM   RF,7,5(R4)          MARKET WEIGHT FOUND -                        
         LA    R1,MKTALL           ACCUMULATE ALL LEVELS OF MARKET TOTS         
         LA    R0,6                                                             
         TM    4(R4),MESTIM        TEST MKT ENCOUNTERED IN THIS ESTIM           
         BO    *+12                                                             
         OI    4(R4),MESTIM+MCLASS  NO - NOW ITS BEEN ENCOUNTERED IN            
         B     MKTFST6                   THIS ESTIM AND THIS CLASS              
         LA    R1,MKTCLS                                                        
         LA    R0,5                                                             
         TM    4(R4),MCLASS        TEST MKT ENCOUNTERED IN THIS CLASS           
         BO    *+12                                                             
         OI    4(R4),MCLASS                                                     
         B     MKTFST6                                                          
         LA    R1,MKTFLM           YES - SKIP ALL CLASSES AND ALL CMLS          
         LA    R0,4                                                             
*                                                                               
MKTFST6  L     RE,0(R1)            AUGMENT NUMBER OF MARKETS                    
         LA    RE,1(RE)                                                         
         ST    RE,0(R1)                                                         
         L     RE,4(R1)            ACCUMULATE MARKET WEIGHT                     
         AR    RE,RF                                                            
         ST    RE,4(R1)                                                         
*                                                                               
MKTFST8  LA    R1,8(R1)                                                         
         BCT   R0,MKTFST6                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* RESOLVE LITERALS                                                              
*                                                                               
DH300    L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         LA    R1,LITS                                                          
*                                                                               
DH310    CLI   0(R1),X'FF'         FIND WHICH LITERAL                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         EX    RE,DH3CLC                                                        
         BE    DH320                                                            
         LA    R1,8(R1)                                                         
         B     DH310                                                            
*                                                                               
DH320    ICM   RF,7,5(R1)                                                       
         BASR  RE,RF                                                            
         STC   R5,GLARGS+1         OUTPUT LENGTH                                
         B     EXIT                                                             
*                                                                               
DH3CLC   CLC   0(0,R2),0(R1)       * EXECUTED                                   
*                                                                               
LITS     DC    CL5'CPP  ',AL3(CPPLIT)                                           
         DC    X'FF'                                                            
*                                                                               
CPPLIT   MVC   0(7,R3),SPACES                                                   
         MVC   2(3,R3),CPPTITL                                                  
         LA    R5,7                                                             
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* FIRST TIME CONTROL                                                            
*                                                                               
DH400    CLC   GLARGS(1),STALEV    STATION LEVEL                                
         BNE   EXIT                                                             
         AP    STATCNTR,=P'1'      AUGMENT STATION COUNTER                      
         XC    DPTGRPSV,DPTGRPSV   CLEAR DPT GROUP SAVE                         
         ZAP   DPTCNTR,=P'0'       INIT DPT COUNTER                             
         ZAP   LENCNTR,=P'0'       INIT LENGTH COUNTER                          
         ZAP   DLCNTR,=P'0'        INIT DPTLEN DETAIL COUNTER                   
         XC    SLCNTRS,SLCNTRS     CLEAR SPOT LENGTH COUNTERS                   
         GOTO1 PRINT,DMCB,P,=C'BL01'    PRINT SPACING LINE                      
         ZIC   RE,LINE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,LINE                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* HEADHOOK                                                                      
*                                                                               
DH600    CLI   CROSSMKT,C'Y'       TEST FOR ALL MARKETS                         
         BNE   EXIT                                                             
         MVC   H5+49(8),=C'MARKETS='    YES - PRINT NUMBER OF MARKETS           
         L     R2,AMKTS                       AND COVERAGE                      
         L     R3,0(R2)                                                         
         L     R4,4(R2)                                                         
         EDIT  (R3),(3,H5+57),ALIGN=LEFT                                        
         MVC   H5+62(4),ASTS                                                    
         MVC   H5+68(9),=C'COVERAGE='                                           
         EDIT  (R4),(6,H5+77),2,ALIGN=LEFT                                      
         L     RE,AMKTS            CLEAR LOWER LEVELS                           
         LA    RF,MKTMG3+8                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         XC    AMKTS,AMKTS         CLEAR MARKET TOTALS POINTER                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* LOOK UP FILM NUMBER AND CLASS                                                 
* INPUT  : FILMSEQ(2)                                                           
* OUTPUT : FILMNUM(8)                                                           
*          CLASS(4)                                                             
*                                                                               
LKUPFILM NTR1                                                                   
         MVC   CLASS,=C'ZZZZ'                                                   
         XC    FILMNUM,FILMNUM                                                  
         MVI   FILMNUM,X'FE'       UNKNOWN FILM                                 
         LA    R5,COMLTAB                                                       
         ICM   R6,15,CMLTBCNT                                                   
         BZ    EXIT                                                             
         GOTO1 BINSRCH,DMCB,(0,FILMSEQ),(R5),(R6),29,(0,2),(R6)                 
         CLI   0(R1),0                                                          
         BNE   EXIT                                                             
         L     RF,0(R1)                                                         
         MVC   CLASS,2(RF)                                                      
         MVC   FILMNUM,6(RF)                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        BUFFALO ROUTINES                                                       
*                                                                               
HIGHBUFF XC    BFKEY,BFKEY                                                      
         LA    R1,=C'HIGH'                                                      
         B     BUFFX                                                            
*                                                                               
SEQBUFF  LA    R1,=C'SEQ'                                                       
         B     BUFFX                                                            
*                                                                               
PUTBUFF  L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         CLC   BUFFSOFA,BUFFCRMX   TEST FOR BUFFER FULL                         
         BL    PB010                                                            
         DROP  RF                                                               
         ST    RE,FULL             YES -                                        
         MVC   BFRECSV,BFREC       SAVE CURRENT REC                             
         BAS   RE,PUTDRIV          EMPTY BUFFER TO DRIVER                       
         MVC   BFREC,BFRECSV       RESTORE CURRENT REC                          
         L     RE,FULL                                                          
*                                                                               
PB010    LA    R1,=C'PUT'                                                       
         B     BUFFX                                                            
*                                                                               
RSETBUFF LA    R1,=C'RESET'                                                     
         B     BUFFX                                                            
*                                                                               
BUFFX    NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 BUFFALO,DMCB,,BUFFBUFF,BFREC,1                                   
         TM    DMCB+8,X'80'                                                     
         B     EXIT                                                             
         EJECT                                                                  
PUTDRIV  NTR1                                                                   
*                                                                               
*        ROUTINE TO PUT RECORDS TO DRIVER                                       
*               AND PUT CROSS MARKETS RECORDS TO DRIVER                         
*                                                                               
         LA    RE,PROGPROF                                                      
         ST    RE,GLAPPROF                                                      
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,HIGHBUFF                                                      
         B     *+8                                                              
*                                                                               
PD010    BAS   RE,SEQBUFF                                                       
         BNE   PD100                                                            
         OC    BFDATA,BFDATA                                                    
         BZ    PD010                                                            
         MVC   GLMAXTLV,BFMAXTLV                                                
         MVI   ANYDATA,C'Y'        INDICATE THERE IS DATA                       
         GOTO1 ADRIVER,DMCB,(RC)                                                
         CLC   BFSTA,XFF           TEST FOR MARKET TOTAL RECORD                 
         BNE   PD010                                                            
         CLI   MKTDATA,C'Y'                                                     
         BE    PD040                                                            
         MVI   MKTDATA,C'Y'        SHOW THERE IS ACTIVITY IN THIS MKT           
         L     R2,AMKTWTAB                                                      
*                                                                               
PD020    OC    0(8,R2),0(R2)       LOOK UP MKT IN MKT WEIGHT TABLE              
         BZ    PD030               NEW ENTRY                                    
         CLC   0(4,R2),MKT                                                      
         BNE   *+14                                                             
         MVC   SPWEIGHT+1(3),5(R2)   EXTRACT THE WEIGHT                         
         B     PD040                                                            
         LA    R2,8(R2)            NEXT MARKET                                  
         C     R2,AMKTWTBX                                                      
         BL    PD020                                                            
         DC    H'0'                                                             
*                                                                               
PD030    GOTO1 =V(VMDADDWT),DMCB,(R8)   GET THE MARKET WEIGHT                   
         MVC   0(4,R2),MKT                                                      
         MVC   5(3,R2),SPWEIGHT+1                                               
*                                                                               
PD040    L     R2,BFSPT                                                         
         L     R4,SPWEIGHT                                                      
         LA    R5,BFSPTWT          WEIGHTED SPOTS                               
         BAS   RE,WEIGHT                                                        
         LA    R0,4                WEIGHT THE DEMOS                             
         L     R1,ADEMLST                                                       
         LA    R5,BFDEM1                                                        
*                                                                               
PD045    LA    RF,2                                                             
         CLI   1(R1),C'R'                                                       
         BE    PD050                                                            
         CLI   SPOTPROF+1,C'D'                                                  
         BE    PD050                                                            
         LA    R5,8(R5)                                                         
         B     PD055                                                            
*                                                                               
PD050    L     R2,0(R5)                                                         
         BAS   RE,WEIGHT                                                        
         LA    R5,4(R5)                                                         
         BCT   RF,PD050                                                         
*                                                                               
PD055    LA    R1,3(R1)                                                         
         BCT   R0,PD045                                                         
*                                                                               
         MVC   MKT,XFF             ALL MARKETS RECORD                           
         GOTO1 ADRIVER,DMCB,(RC)                                                
         SR    R2,R2                                                            
         ICM   R2,1,SDNMGRPS                                                    
         BZ    PD070                                                            
*                                                                               
PD060    STC   R2,MGTOT            ALL MARKET GROUPS                            
         GOTO1 ADRIVER,DMCB,(RC)                                                
         BCT   R2,PD060                                                         
*                                                                               
PD070    TM    CMLSW,FLM           TEST FOR COMMERCIALS                         
         BZ    PD080                                                            
         MVC   BFFILM,XFF          YES - ALL COMMERCIALS                        
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
PD080    TM    CMLSW,CLS           TEST FOR COMMERCIAL CLASS                    
         BZ    PD090                                                            
         CLI   QFILTER+1,C' '      YES - TEST ONLY ONE CLASS                    
         BNE   PD090                                                            
         MVC   BFCLASS,XFF               NO - ALL CLASSES                       
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
PD090    MVC   MKT,MKTSV           RESTORE MARKET                               
         MVI   MGTOT,0                                                          
         B     PD010                                                            
*                                                                               
PD100    BAS   RE,RSETBUFF                                                      
         B     EXIT                                                             
         SPACE 2                                                                
WEIGHT   SRDA  R2,32                                                            
         MR    R2,R4               MULTIPLY BY WEIGHT                           
         ST    R3,0(R5)                                                         
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*STORGE*'                                                    
DOL      DS    PL8                                                              
DOLEQ    DS    PL8                                                              
COVAIL   DC    V(COVAIL)                                                        
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
AWKAREA  DC    A(WKAREA)                                                        
AMKTWTAB DC    A(MKTWTAB)                                                       
AMKTWTBX DC    A(MKTWTABX)                                                      
SVAREC   DS    A                                                                
AMKTS    DS    A                                                                
ADEMLST  DS    A                                                                
RATING   DS    F                                                                
RATINGEQ DS    F                                                                
CMLTBCNT DS    F                                                                
TOTWGT   DS    F                                                                
*                                                                               
DPGFILE  DC    CL8'SPMC05  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
XFF      DC    20X'FF'                                                          
ASTS     DC    10C'*'                                                           
PSLIST   DS    XL150               PRODUCT SPOT LENGTH LIST                     
FILMSEQ  DS    XL2                                                              
CANISTER DS    CL8                                                              
FILMNAME DS    CL15                                                             
SVKEY    DS    XL13                                                             
SVKEYSV  DS    XL13                                                             
STATCNTR DS    PL2                                                              
DPTCNTR  DS    PL2                                                              
LENCNTR  DS    PL2                                                              
DLCNTR   DS    PL2                                                              
ELCODE   DS    XL1                                                              
PRTSW    DS    CL1                                                              
STALEV   DS    XL1                                                              
ANYDATA  DS    CL1                                                              
CROSSDPT DS    CL1                                                              
DPTOT    DS    CL1                                                              
EQUIV    DS    CL1                                                              
TOTLEV   DS    XL1                                                              
GETBYP2  DS    XL1                                                              
MGTOT    DS    XL1                                                              
MKTDATA  DS    CL1                                                              
CROSSMKT DS    CL1                                                              
FIRSTSW  DS    CL1                                                              
SECBRAND DS    CL1                                                              
CPPTITL  DS    CL3                                                              
MKTSV    DS    CL4                                                              
DPTGRPSV DS    CL4                                                              
CLASS    DS    CL4                                                              
FILMNUM  DS    XL8                                                              
SVFILM   DS    XL2                                                              
SVFLMNUM DS    XL8                                                              
SVMARKET DS    CL4                                                              
DMNAMES  DC    XL28'00'            4 DEMO NAMES                                 
PRDSV    DS    X                                                                
BUYKEYSV DS    XL13                                                             
*                                                                               
CMLSW    DS    XL1                                                              
FLM      EQU   X'80'                                                            
CLS      EQU   X'40'                                                            
*                                                                               
SLCNTRS  DS    0CL15                                                            
         DS    X                                                                
         DS    PL2                                                              
         DS    X                                                                
         DS    PL2                                                              
         DS    X                                                                
         DS    PL2                                                              
         DS    X                                                                
         DS    PL2                                                              
         DS    X                                                                
         DS    PL2                                                              
*                                                                               
         DS    0F                                                               
MKTCNTRS DS    0XL48                                                            
MKTALL   DS    XL8                                                              
MKTCLS   DS    XL8                                                              
MKTFLM   DS    XL8                                                              
MKTMG1   DS    XL8                                                              
MKTMG2   DS    XL8                                                              
MKTMG3   DS    XL8                                                              
*                                                                               
CTAB     DS    XL8                                                              
FILMLST  DS    200X                                                             
FILMLSTX EQU   *                                                                
FILMLSTL EQU   FILMLSTX-FILMLST                                                 
*                                                                               
         DS    0F                                                               
         DC    CL8'*BUFREC*'                                                    
BFREC    DS    0CL68                                                            
*                                                                               
BFKEY    DS    0CL20                                                            
BFPRD    DS    CL1                                                              
BFFILM   DS    XL2                                                              
BFCLASS  DS    CL4                                                              
BFSTA    DS    CL3                                                              
BFDPTLEN DS    0CL9                                                             
BFDPTGRP DS    CL4                 DAYPART GROUP NO(1)/CODE(3)                  
BFDPT    DS    CL4                 DAYPART NO(1)/CODE(3)                        
BFLEN    DS    CL1                                                              
BFMAXTLV DS    CL1                                                              
*                                                                               
BFDATA   DS    0CL48                                                            
BFSPT    DS    XL4                                                              
BFSPTWT  DS    XL4                                                              
BFDOL    DS    XL4                                                              
BFDOLEQ  DS    XL4                                                              
BFDEM1   DS    XL4                                                              
BFDEM1EQ DS    XL4                                                              
BFDEM2   DS    XL4                                                              
BFDEM2EQ DS    XL4                                                              
BFDEM3   DS    XL4                                                              
BFDEM3EQ DS    XL4                                                              
BFDEM4   DS    XL4                                                              
BFDEM4EQ DS    XL4                                                              
*                                                                               
BFRECSV  DS    CL68                                                             
         EJECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*CMLTAB*'                                                    
COMLTAB  DS    1000XL29            COMMERCIAL TABLE                             
*                                  ENTRY : +00(02) FILM SEQ                     
*                                          +02(04) CLASS                        
*                                          +06(08) FILM NUMBER                  
*                                          +14(15) FILM NAME                    
COMLTABX EQU   *                                                                
*                                                                               
         DC    CL8'DRGLOBAL'                                                    
GLAREA   DS    40000X              DRIVER GLOBAL AREA                           
*                                                                               
         DC    CL8'*MKTWTS*'                                                    
MKTWTAB  DS    500XL8              MARKET WEIGHT TABLE                          
*                                  ENTRY : +0(4) MARKET                         
*                                          +4(1) MARKET ENCOUNTERED SW          
*                                          +5(3) MARKET WEIGHT                  
MKTWTABX EQU   *                                                                
MESTIM   EQU   X'80'               MARKET ENCOUNTERED SWITCH VALUES             
MCLASS   EQU   X'40'                                                            
FF       EQU   X'FF'                                                            
*                                                                               
         DC    CL8'*WKAREA*'                                                    
WKAREA   DS    2600X                                                            
*                                                                               
         BUFF  LINES=1,ROWS=1,COLUMNS=12,FLAVOR=BINARY,KEYLIST=(20,A)           
         EJECT                                                                  
PLINED   DSECT                                                                  
         DS    CL48                                                             
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL4                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL3                                                              
PLN      DS    CL3                                                              
         DS    CL48                                                             
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
       ++INCLUDE SPDRVWRKD                                                      
*        PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPTRCMML                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028SPREPPE02 05/01/02'                                      
         END                                                                    
