*          DATA SET SPREPPX02  AT LEVEL 091 AS OF 05/01/02                      
*          DATA SET SPREPPX02  AT LEVEL 006 AS OF 10/21/88                      
*PHASE SPPX02A                                                                  
         TITLE 'SPREPPX02 - MEDIA PLANNING AGENCY SPOT EXTRACT'                 
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
*        QOPT1   A=ARB,M=NSI (FILTER FOR MULTI-CLIENT RUNS)                     
*        QOPT2   DPG PHASE TO USE - (A THRU D = DOWNLOAD)                       
*        QOPT3   Y=30'S ONLY                                                    
*        QOPT4   QUARTER NUMBER                                                 
*        QOPT5   T=USE TEST DAYPART LIST                                        
*                                                                               
SPPX02   CSECT                                                                  
         NMOD1 0,SPPX02,RR=R5                                                   
*                                                                               
         L     R9,0(R1)                                                         
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,R9,RA                                                    
         LA    R8,GENSUBS                                                       
         USING GENSUBS,R8                                                       
         ST    R5,RELO                                                          
*                                                                               
         L     RC,=A(GLAREA)                                                    
         A     RC,RELO                                                          
         ST    RC,VGLAREA                                                       
         USING GLOBALD,RC                                                       
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
         CLI   MODE,STALAST                                                     
         BE    STAL                                                             
         CLI   MODE,MKTFRST                                                     
         BE    MKTF                                                             
         CLI   MODE,MKTLAST                                                     
         BE    MKTL                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,PRDFRST                                                     
         BE    PRDF                                                             
         CLI   MODE,PRDLAST                                                     
         BE    PRDL                                                             
         CLI   MODE,CLTLAST                                                     
         BE    CLTL                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         B     EXIT                                                             
         SPACE 2                                                                
* RUN FIRST                                                                     
*                                                                               
RUNF     DS    0H                                                               
         MVI   REQFSW,0                                                         
         XC    ADRIVER,ADRIVER                                                  
         XC    ASYSDRV,ASYSDRV                                                  
         XC    AMKTTAB,AMKTTAB                                                  
*                                                                               
         L     RF,=A(SPHOOK)                                                    
         A     RF,RELO                                                          
         ST    RF,SPOTHOOK                                                      
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
* REQUEST FIRST                                                                 
*                                                                               
REQF     DS    0H                                                               
         CLI   REQFSW,0            FIRST TIME SW                                
         BNE   REQF50                                                           
         MVI   REQFSW,1                                                         
*                                                                               
         MVC   SVQCLT,QCLT         SET QSAVES                                   
         MVC   SVQPRD,QPRD                                                      
         MVC   SVQEST,QEST                                                      
         MVC   SVQSTRT,QSTART                                                   
         MVC   SVQEND,QEND                                                      
         MVC   SVQMENU,QDEMNOS                                                  
         MVC   SVQQTR,QOPT4        SAVE QUARTER                                 
         MVI   FLACTSW,C'N'        CLEAR FILE ACTIVE SW                         
*                                                                               
         MVI   RQDAYPT,C'Y'        READ DAYPART MENU                            
         MVI   RQEQUIV,C'Y'        READ EQUIVALENCES                            
*                                                                               
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
         MVI   ESTFSW,0            SET FIRST TIME SW FOR EST                    
*                                                                               
         CLC   QEST,=C'ALL'                                                     
         BNE   *+10                                                             
         MVC   QEST,=C'NO '                                                     
*                                                                               
         CLC   SVQCLT,QCLT         TEST SAME CLIENT                             
         BE    *+8                                                              
         MVI   SVQCLT,X'FF'        SET VARIOUS                                  
*                                                                               
         CLC   SVQPRD,QPRD         TEST SAME PRODUCT                            
         BE    *+8                                                              
         MVI   SVQPRD,X'FF'        SET VARIOUS                                  
*                                                                               
         CLC   SVQEST,QEST         TEST SAME EST                                
         BE    *+8                                                              
         MVI   SVQEST,X'FF'        SET VARIOUS                                  
*                                                                               
         CLC   SVQSTRT,QSTART      TEST START                                   
         BNH   *+10                                                             
         MVC   SVQSTRT,QSTART      SAVE LOWEST                                  
*                                                                               
         CLC   SVQEND,QEND         TEST END                                     
         BNL   *+10                                                             
         MVC   SVQEND,QEND         SAVE HIGHEST                                 
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        CLIENT FIRST                                                           
*                                                                               
CLTF     DS    0H                                                               
*                                  RATING SOURCE                                
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVI   SRCE,C'N'           A=ARB, N=NSI                                 
         CLI   CPROF+3,C'0'                                                     
         BE    *+8                                                              
         MVI   SRCE,C'A'                                                        
*                                                                               
         CLI   QOPT1,C' '          TEST SOURCE FILTER                           
         BE    CLTF3                                                            
         CLC   QOPT1,SRCE                                                       
         BE    CLTF3                                                            
         CLC   QCLT,=C'ALL'        UNLESS ALL CLIENT RUN                        
         BNE   CLTF3D              PRINT ERROR                                  
         MVI   MODE,CLTLAST        ELSE, JUST SKIP CLIENT                       
         B     EXIT                                                             
*                                  TEST MIXED SOURCES                           
CLTF3    DS    0H                                                               
         CLI   SVQSRCE,0                                                        
         BNE   *+10                                                             
         MVC   SVQSRCE,SRCE                                                     
         CLC   SVQSRCE,SRCE                                                     
         BE    CLTF4                                                            
*                                                                               
CLTF3D   DS    0H                                                               
         MVC   P(41),=C'**CONFLICTING RATING SOURCES- SKIP CLIENT'              
         MVC   P+42(3),CLT                                                      
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
CLTF4    DS    0H                                                               
         MVI   ESTFSW,0            SET FIRST TIME SW FOR EST                    
         B     EXIT                                                             
         SPACE 2                                                                
*        PRODUCT FIRST                                                          
*                                                                               
PRDF     DS    0H                                                               
         MVI   ESTFSW,0            SET FIRST TIME SW FOR EST                    
         B     EXIT                                                             
         SPACE 2                                                                
*        ESTIMATE FIRST                                                         
*                                                                               
ESTF     DS    0H                                                               
         CLI   ESTFSW,0            TEST FIRST TIME                              
         BNE   ESTF40              NO                                           
         MVI   ESTFSW,1                                                         
*                                  YES- DO SOME INITIALIZATION                  
*                                                                               
         MVI   SPOTPROF+2,0        FORCE TO BROADCAST MONTHS                    
         MVI   SPOTPROF+8,1        AND MONDAY STARTS                            
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
*                                                                               
         MVC   MEDNUMPE,=F'1'      NO. PERIODS                                  
         MVC   MEDNUMMO,=F'13'     NO. MONTHS                                   
         MVC   MEDNUMQT,=F'4'      NO. QTRS                                     
         MVC   MEDLCHNK,=F'200'    CHUNK LENGTH                                 
*                                                                               
         MVC   QDEMNOS,SVQMENU     FORCE SAME DEMO MENU                         
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
         GOTO1 MEDPRDRD,DMCB,SPWORKD   NOTE- DEMOS WILL BE SET FROM             
*                                      DEMO MENU ON REQ                         
         LA    RF,219              POL                                          
         CLI   BPRD,X'FF'                                                       
         BE    ESTF2F                                                           
         CLI   BPRD,0                                                           
         BE    ESTF2F                                                           
         ZIC   RF,BPRD            OR PRD                                        
         BCTR  RF,R0                                                            
*                                                                               
ESTF2F   DS    0H                                                               
         L     R4,PRDBUFF          FIND POL OR PRD ENTRY                        
         MH    RF,PRDBUFLN                                                      
         AR    R4,RF                                                            
         USING PTBUFFD,R4                                                       
*                                                                               
         CLI   PTDEMO+2,1          FIRST DEMO MUST BE HH'S                      
         BE    ESTF3                                                            
         MVC   P(40),=C'** FIRST DEMO IN MENU MUST BE HOUSEHOLDS **'            
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
ESTF3    DS    0H                                                               
         LA    R0,NDEMS            COUNT DEMOS (NDEMS MAX)                      
         LA    R3,PTDEMO                                                        
*                                                                               
ESTF4    DS    0H                                                               
         OC    0(3,R3),0(R3)       TEST EOL                                     
         BZ    ESTF6                                                            
         LA    R3,3(R3)                                                         
         BCT   R0,ESTF4                                                         
*                                                                               
ESTF6    DS    0H                                                               
         LA    R1,7                                                             
         SR    R1,R0               R1 HAS COUNT OF DEMOS                        
         ST    R1,NADEMS           SAVE NUMBER OF ACTUAL DEMOS                  
         LR    R2,R1                                                            
         SLL   R2,1                X 2 FOR RATINGS                              
         STC   R2,MEDEXTDM         SET NO. DEMOS TO EXTRACT                     
*                                                                               
*                                  FORCE TO IMPS, COPY AS RTGS                  
*                                  TO 2ND HALF OF POL LIST                      
*                                  AND COPY WHOLE LIST TO DEMLST                
*                                                                               
         LA    R3,PTDEMO                                                        
         LR    RF,R1                                                            
         MH    RF,=H'3'                                                         
         LA    R5,0(R3,RF)         R5 TO 2ND HALF                               
         LA    R2,DEMLST                                                        
*                                                                               
ESTF8    DS    0H                                                               
         MVI   1(R3),C'I'          FORCE TO IMPS(00)                            
         MVC   0(3,R5),0(R3)       COPY TO 2ND HALF                             
         MVI   1(R5),C'R'          RATING                                       
         MVC   0(3,R2),0(R3)       COPY TO DEMLST                               
         MVI   1(R2),C'L'          RESET TO POP(00)                             
*                                                                               
         LA    R2,3(R2)                                                         
         LA    R3,3(R3)                                                         
         LA    R5,3(R5)                                                         
         BCT   R1,ESTF8                                                         
*                                                                               
         XC    0(3,R5),0(R5)       CLEAR NEXT ENTRY                             
         MVC   0(3,R2),=3X'FF'                                                  
*                                  SET QTRTAB                                   
         MVC   QTRTAB,=X'FEFEFEFEFEFF'  SET IGNORES AND EOL                     
*                                  ***FOR NOW SET ALL AS                        
*                                  QTR 0 (TOTAL) **                             
         MVI   QTRTAB,0            SET TOTAL QTR ACTIVE                         
*                                                                               
ESTF9    DS    0H                                                               
*                                  GET MARKET NAMES                             
*                                  ----------------                             
ESTF21   DS    0H                                                               
         OC    AMKTTAB,AMKTTAB     DONT DO MORE THAN ONCE                       
         BNZ   ESTF24                                                           
         BAS   RE,BLDMTAB                                                       
         SPACE 2                                                                
*                                  SAVE MKT UNIVERSES                           
*                                  ------------------                           
         L     R4,ADBLOCK                                                       
         USING DBLOCK,R4                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELAGY,QAGY                                                    
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,SRCE       RATING SOURCE                                
         PACK  DUB,QBOOK1(2)       CONVERT BOOK                                 
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK                                                       
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK+1                                                     
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,ADBUY                                                         
         ST    RE,DBAREC                                                        
         MVI   DBSELDAY,X'40'                                                   
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1715'                                           
         MVI   DBFUNCT,DBGETTOT                                                 
*                                                                               
         L     R6,AMKTTAB                                                       
         LA    R6,MKLEN(R6)        BUMP PAST US TOTALS                          
         USING MKTTABD,R6                                                       
         SPACE 2                                                                
ESTF23   DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    ESTF24                                                           
*                                                                               
         MVC   DBSELRMK,MKNUM                                                   
         GOTO1 DEMAND,DMCB,ADBLOCK,SVUNIV                                       
*                                                                               
         L     R5,AMKTTAB          US TOTALS                                    
         LA    R5,MKUNVS-MKTTABD(R5)                                            
         LA    R3,MKUNVS                                                        
         LA    R0,NDEMS                                                         
*                                                                               
ESTF23D  DS    0H                                                               
         L     R1,0(R3)            MKT POP                                      
         A     R1,0(R5)                                                         
         ST    R1,0(R5)            US POP                                       
*                                                                               
         LA    R3,4(R3)            NEXT DEMO                                    
         LA    R5,4(R5)                                                         
         BCT   R0,ESTF23D                                                       
*                                                                               
         LA    R6,MKLEN(R6)                                                     
         B     ESTF23                                                           
         DROP  R6                                                               
*                                  GET DEMO NAMES                               
ESTF24   DS    0H                                                               
         L     R2,NADEMS                                                        
         GOTO1 DEMOCON,DMCB,((R2),DEMLST),(6,DNAMES),                  X        
               (C'S',ADBLOCK),ADEST                                             
*                                                                               
         LA    R1,DNAMES                                                        
ESTF25   DS    0H                                                               
         MVI   0(R1),C' '          STRIP OFF 'H'                                
         CLI   1(R1),C'A'          CHANGE A TO V                                
         BNE   *+8                                                              
         MVI   1(R1),C'V'                                                       
         LA    R1,6(R1)                                                         
         BCT   R2,ESTF25                                                        
*                                                                               
ESTF40   DS    0H                                                               
         B     EXIT                                                             
         DROP  R7                                                               
         SPACE 2                                                                
*        STATION LAST                                                           
*                                                                               
STAL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        MARKET FIRST                                                           
*                                                                               
MKTF     DS    0H                                                               
         L     RF,ADCLT            SET RATING SOURCE MARKET                     
         LA    RF,CPROF+3-CLTHDR(RF)   RATING SOURCE                            
         L     RE,ADMARKET                                                      
         LA    RE,MKTRS1-MKTREC(RE)                                             
         CLC   0(1,RF),0(RE)                                                    
         BE    *+8                                                              
         LA    RE,MKTRS2-MKTRS1(RE)    POINT TO NEXT SERVICE                    
         MVC   RSMKT,1(RE)         SET MKT                                      
*                                                                               
         OC    RSMKT,RSMKT                                                      
         BZ    MKTF3                                                            
         MVC   MW(2),RSMKT                                                      
         GOTO1 BINSRCH,MKTPARS,MW                                               
         CLI   0(R1),1                                                          
         BNE   MKTF4                                                            
MKTF3    DS    0H                                                               
         TM    GLDOWNLD,X'80'      NO MESSAGE IF DOWNLOADING                    
         BNZ   MKTF3D                                                           
         MVC   P(59),=C'**BAD RATING SERVICE MARKET - NNNN, AGENCY MARKX        
               ET IS NNNN**'                                                    
         EDIT  (B2,RSMKT),(4,P+30),ZERO=NOBLANK                                 
         MVC   P+53(4),MKT                                                      
         GOTO1 REPORT                                                           
*                                                                               
MKTF3D   DS    0H                                                               
         XC    RSMKT,RSMKT                                                      
         B     EXIT                                                             
*                                                                               
MKTF4    DS    0H                                                               
         L     R6,0(R1)                                                         
         ST    R6,SVADMKT          SAVE ADRESS OF MKT ENTRY                     
         B     EXIT                                                             
         SPACE 2                                                                
*        MARKET LAST                                                            
*                                                                               
MKTL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRODUCT LAST                                                           
*                                                                               
PRDL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        CLIENT LAST                                                            
*                                                                               
CLTL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        RUN LAST                                                               
         SPACE 2                                                                
RUNL     DS    0H                                                               
         CLI   FLACTSW,C'Y'        TEST FILE ACTIVE                             
         BNE   EXIT                                                             
*                                                                               
         L     R6,AMKTTAB          PUT MKT TOTALS TO DRIVER                     
         LA    R6,MKLEN(R6)        FIRST REAL MARKET                            
         USING MKTTABD,R6                                                       
*                                                                               
RUNL2    DS    0H                                                               
         CLI   0(R6),X'FF'         END OF MKTS                                  
         BE    RUNL8                                                            
         LA    R4,MKDPQW           TEST MKT ACTIVE                              
         LA    R5,NDPTS*NQTRS                                                   
*                                                                               
RUNL4    DS    0H                                                               
         OC    0(4,R4),0(R4)       ANY SPOTS                                    
         BNZ   RUNL6               YES - MKT IS ACTIVE                          
         LA    R4,MKDPDL(R4)       NEXT DPT/QTR                                 
         BCT   R5,RUNL4                                                         
         B     RUNL7                                                            
*                                                                               
RUNL6    DS    0H                                                               
         ST    R6,SVADMKT                                                       
         MVI   DRPASS,2            MKT TOTAL PASS                               
         BAS   RE,DRVIN            DO DRIVER INPUT                              
*                                                                               
RUNL7    DS    0H                                                               
         LA    R6,MKLEN(R6)        NEXT MARKET                                  
         B     RUNL2                                                            
*                                                                               
         DROP  R6                                                               
RUNL8    DS    0H                                                               
         L     R2,AMKTTAB          ROLL TO US TOTALS                            
         USING MKTTABD,R2                                                       
         LA    R2,MKLEN(R2)        BUMP TO FIRST MKT                            
         MVC   AMKTCNT,=F'1'       ACTIVE MKT COUNT (INCLUDE US)                
*                                                                               
RM4      DS    0H                                                               
         CLI   0(R2),X'FF'         END OF MKTS                                  
         BE    RM16                                                             
         MVI   MKACTSW,0                                                        
         L     R3,AMKTTAB          US TOTALS                                    
         LA    R3,MKDPQW-MKTTABD(R3) DPT/Q WORK                                 
         LA    R4,MKDPQW           THIS MKTS DPT/Q AREA                         
         LA    R5,NDPTS*NQTRS                                                   
*                                                                               
RM6      DS    0H                                                               
         OC    0(4,R4),0(R4)       TEST ANY SPOTS FOR DPT/QTR                   
         BNZ   RM7                                                              
         LA    R3,MKDPDL(R3)       NEXT DPT/Q - US                              
         LA    R4,MKDPDL(R4)       NEXT DPT/Q - MKT                             
         B     RM10                                                             
*                                                                               
RM7      DS    0H                                                               
         MVI   MKACTSW,1           SET MKT ACTIVE                               
         L     R0,0(R3)            SPOTS                                        
         A     R0,0(R4)                                                         
         ST    R0,0(R3)                                                         
         L     R0,4(R3)            $                                            
         A     R0,4(R4)                                                         
         ST    R0,4(R3)                                                         
         L     R0,8(R3)            IMPS                                         
         A     R0,8(R4)                                                         
         ST    R0,8(R3)                                                         
*                                                                               
         LA    R3,MKDPGRPS-MKDPD(R3)    FIRST GRP                               
         LA    R4,MKDPGRPS-MKDPD(R4)                                            
         LA    R6,NDEMS            DEMO COUNT FOR BCT                           
*                                                                               
RM8      DS    0H                                                               
         L     R1,0(R4)            MKT GRPS                                     
         M     R0,NDEMS*4(R4)      POPS ARE AFTER GRPS                          
         LA    RF,1000                                                          
         BAS   RE,DIV                                                           
         A     R1,0(R3)            ADD TO US TOT                                
         ST    R1,0(R3)                                                         
         L     R0,NDEMS*4(R4)      ADD POP                                      
         A     R0,NDEMS*4(R3)                                                   
         ST    R0,NDEMS*4(R3)                                                   
*                                                                               
         LA    R3,4(R3)            NEXT ITEM                                    
         LA    R4,4(R4)                                                         
         BCT   R6,RM8                                                           
*                                                                               
         LA    R3,L'MKDPUNVS(R3)   POSITION TO NEXT DPT/QTR                     
         LA    R4,L'MKDPUNVS(R4)                                                
*                                                                               
RM10     DS    0H                                                               
         BCT   R5,RM6              NEXT DPT/Q                                   
*                                                                               
         CLI   MKACTSW,1           COUNT ACTIVE MKTS                            
         BNE   RM12                                                             
         L     RF,AMKTCNT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,AMKTCNT                                                       
*                                                                               
RM12     DS    0H                                                               
         LA    R2,MKLEN(R2)        NEXT MKT                                     
         B     RM4                                                              
*                                                                               
RM16     DS    0H                  DONE ROLLING - NOW RE-DIVIDE                 
         L     R2,AMKTTAB                                                       
         LA    R5,NDPTS*NQTRS                                                   
         LA    R3,MKDPQW                                                        
         USING MKDPD,R3                                                         
*                                                                               
RM18     DS    0H                                                               
         LA    R4,MKDPGRPS                                                      
         LA    R6,NDEMS                                                         
*                                                                               
RM20     DS    0H                                                               
         L     R1,0(R4)                                                         
         M     R0,=F'1000'                                                      
         L     RF,NDEMS*4(R4)      / POP                                        
         BAS   RE,DIV                                                           
         ST    R1,0(R4)            RESET GRP                                    
*                                                                               
         LA    R4,4(R4)                                                         
         BCT   R6,RM20                                                          
*                                                                               
         LA    R3,MKDPDL(R3)        NEXT DPT/QTR                                
         BCT   5,RM18                                                           
*                                                                               
         DROP  R2                                                               
         DROP  R3                                                               
*                                                                               
         L     R6,AMKTTAB                                                       
         ST    R6,SVADMKT                                                       
         XC    RSMKT,RSMKT                                                      
         MVI   DRPASS,2            'MKT TOTAL' PASS                             
         BAS   RE,DRVIN            DO DRIVER INPUT FOR US TOTALS                
*                                                                               
*                                  DRIVER OUTPUT PHASE                          
*                                  -------------------                          
         MVI   FORCEHED,C'Y'                                                    
         CLI   SVQCLT,X'FF'        TEST MIXED CLIENTS                           
         BE    RM26                                                             
         CLC   =C'ALL',SVQCLT      OR CLIENT ALL                                
         BE    RM26                                                             
         MVI   MODE,CLTLAST        RESET SPONSOR MODE                           
         CLI   SVQPRD,X'FF'        TEST MIXED PRODUCTS                          
         BE    RM27                                                             
         CLC   =C'ALL',SVQPRD      OR PRODUCT ALL                               
         BE    RM27                                                             
         MVI   MODE,PRDLAST        RESET SPONSOR MODE                           
         CLI   SVQEST,X'FF'        TEST MIXED ESTS                              
         BE    RM28                                                             
         CLC   =C'ALL',SVQEST                                                   
         BE    RM28                                                             
         B     RM30                                                             
*                            CLEAR RECORDS TO GET PROPER HEADLINES              
RM26     MVC   CLT,SPACES                                                       
         MVC   CLTNM,SPACES                                                     
RM27     MVC   PRODUCT,SPACES                                                   
         MVC   PRDNM,SPACES                                                     
RM28     MVC   ESTIMATE,SPACES                                                  
         MVC   ESTNM,SPACES                                                     
*                                                                               
RM30     DS    0H                                                               
         MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
DRVIN    NTR1                                                                   
*                                  DRIVER INPUT PHASE                           
*                                  ------------------                           
         L     R6,SVADMKT                                                       
         USING MKTTABD,R6                                                       
         LA    R2,QTRTAB           TABEL OF QUARTER EQUIVS                      
*                                                                               
DRVIN2   DS    0H                                                               
         CLI   0(R2),X'FE'         TST QTR ACTIVE                               
         BNL   DRVIN3                                                           
         MVC   QTRNO,0(R2)                                                      
         MVI   GLMODE,GLINPUT      SET UP FOR DRIVER INPUT                      
         MVI   ACTRECSW,0          CLEAR ACTIVE SW                              
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
DRVIN3   DS    0H                                                               
         CLI   0(R2),X'FF'         TEST DONE WITH FINAL QTR                     
         BE    DRVIN6                                                           
         LA    R2,1(R2)                                                         
         B     DRVIN2                                                           
*                                                                               
DRVIN6   DS    0H                                                               
         CLC   NADEMS,=F'1'        MUST HAVE DEMOS (OTHER                       
         BNH   DRVINX               THAN HH)                                    
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BZ    DRVINX                                                           
         MVI   DRPASS,C'D'         SET DCF PASS                                 
         LA    R2,QTRTAB           QURTER TABLE                                 
*                                                                               
DRVIN6B  DS    0H                                                               
         CLI   0(R2),X'FE'         TEST QTR ACTIVE                              
         BNL   DRVIN8                                                           
         MVC   QTRNO,0(R2)                                                      
         LA    R3,1                START WITH DEM 1 (SKIP HH)                   
*                                                                               
DRVIN7   DS    0H                                                               
         STC   R3,DEMNO                                                         
         MVI   GLMODE,GLINPUT                                                   
         MVI   ACTRECSW,0          CLEAR ACTIVE SW                              
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         LA    R3,1(R3)                                                         
         C     R3,NADEMS           TEST VS ACTUAL DEMO COUNT                    
         BL    DRVIN7              NOTE- DONT COUNT HH                          
*                                                                               
DRVIN8   DS    0H                                                               
         CLI   0(R2),X'FF'         TEST END OF QTRS                             
         BE    DRVINX                                                           
         LA    R2,1(R2)                                                         
         B     DRVIN6B                                                          
*                                                                               
*                                                                               
DRVINX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        PROCESS BUY                                                            
         SPACE 2                                                                
PRBUY    DS    0H                                                               
         GOTO1 =A(PROCB),RR=RELO                                                
         B     EXIT                                                             
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
         B     DH190                                                            
DH104    DS    0H                                                               
         CLI   GLRECNO,2           FOR RECORD 2                                 
         BNE   DH106                                                            
         CLI   DRPASS,C'D'         MUST BE DCF PASS                             
         BNE   EXIT                                                             
         B     DH190                                                            
*                                                                               
DH106    DS    0H                                                               
         CLI   DRPASS,C'D'         SKIP OTHER RECS FOR DCF PASS                 
         BE    EXIT                                                             
         B     DH190               ***NO-OP QUARTERLY TEST***                   
         CLI   GLRECNO,4           FOR REC 4 (SPOTS)                            
         BE    DH190               DO FOR ALL QTRS                              
         CLI   QTRNO,0             ELSE DO ONLY FOR TOTAL                       
         BNE   EXIT                                                             
*                                                                               
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
         SLL   RF,2                                                             
         L     R1,MKUNVS-4(RF)                                                  
         ST    R1,0(R3)                                                         
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
         MH    RF,=H'6'                                                         
         LA    RF,DNAMES(RF)                                                    
         MVC   0(5,R3),1(RF)       LAST 5 POSITIONS HAVE DEMO                   
         B     EXIT                                                             
*                                                                               
*                                                                               
DLHQTR   DS    0H                  QUARTER                                      
         MVC   0(1,R3),SVQQTR                                                   
         NI    0(R3),X'0F'         MAKE BINARY                                  
         B     EXIT                                                             
*                                                                               
SPTSIP   DS    0H                  SPOTS                                        
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         USING MKDPD,R4                                                         
         OC    0(4,R3),MKDPSPT                                                  
         BZ    *+8                                                              
         MVI   ACTRECSW,1          SET ACTIVE                                   
         B     EXIT                                                             
*                                                                               
COSTIP   DS    0H                  COST                                         
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         OC    0(4,R3),MKDPCST                                                  
         BZ    *+8                                                              
         MVI   ACTRECSW,1          SET ACTIVE                                   
         B     EXIT                                                             
*                                                                               
IMPSIP   DS    0H                  IMPS                                         
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         OC    0(4,R3),MKDPIMP                                                  
         BZ    *+8                                                              
         MVI   ACTRECSW,1          SET ACTIVE                                   
         B     EXIT                                                             
*                                                                               
GRPSIP   DS    0H                  GRPS                                         
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         ZIC   RF,GLARGS+1         2ND ARG IS DEMO                              
         SLL   RF,2                X 4                                          
         L     R1,MKDPGRPS-4(RF)                                                
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         ST    R1,0(R3)                                                         
         MVI   ACTRECSW,1          SET ACTIVE                                   
         B     EXIT                                                             
*                                                                               
DCFIN    DS    0H                  DCF'S                                        
         BAS   RE,MDPQDSP          GET DISPLACEMENT IN R4                       
         ZIC   RF,DEMNO                                                         
         SLL   RF,2                                                             
         L     R1,MKDPGRPS(RF)                                                  
         M     R0,=F'10000'                                                     
         L     RF,MKDPGRPS         HH GRPS                                      
         BAS   RE,DIV                                                           
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         ST    R1,0(R3)            SET DFC                                      
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
         MH    RF,=H'6'                                                         
         LA    RF,DNAMES(RF)                                                    
         MVC   0(5,R3),1(RF)       LAST 5 POSITIONS HAVE DEMO                   
         B     EXIT                                                             
*                                                                               
*                                                                               
DLHTTL   DS    0H                  DOWNLOAD HEADER                              
         MVC   0(8,R3),=C'STVAGY  '                                             
         MVI   ACTRECSW,1          HEADER REC ALWAYS ACTIVE                     
         B     EXIT                                                             
*                                                                               
DLHSRC   DS    0H                                                               
         MVC   0(4,R3),=C'ARB '    SOURCE                                       
         CLI   SRCE,C'A'                                                        
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
         MVC   0(3,R3),SVQCLT                                                   
         CLI   SVQCLT,X'FF'        TEST VARIOUS                                 
         BNE   *+10                                                             
         MVC   0(6,R3),=C'VARIOUS'                                              
         B     EXIT                                                             
*                                                                               
DLHPRD   DS    0H                  PRODUCT                                      
         MVC   0(3,R3),SVQPRD                                                   
         CLI   SVQPRD,X'FF'        TEST VARIOUS                                 
         BNE   *+10                                                             
         MVC   0(6,R3),=C'VARIOUS'                                              
         B     EXIT                                                             
*                                                                               
DLHEST   DS    0H                  ESTIMATE                                     
         MVC   0(6,R3),SVQEST      QEST AND QESTEND                             
         CLI   SVQEST,X'FF'        TEST VARIOUS                                 
         BNE   *+10                                                             
         MVC   0(6,R3),=C'VARIOUS'                                              
         B     EXIT                                                             
*                                                                               
DLHSTD   DS    0H                  START DATE                                   
         MVC   0(6,R3),SVQSTRT                                                  
         B     EXIT                                                             
*                                                                               
DLHEND   DS    0H                  END DATE                                     
         MVC   0(6,R3),SVQEND                                                   
         B     EXIT                                                             
*                                                                               
DLHNDEM  DS    0H                  NUMBER OF DEMOS (EXCLUDING HH)               
         L     R1,NADEMS                                                        
         BCTR  R1,R0                                                            
         STC   R1,0(R3)                                                         
         B     EXIT                                                             
*                                                                               
DLHNMKT  DS    0H                  NUMBER OF MARKETS                            
         MVC   0(2,R3),MKTCNT+2                                                 
         B     EXIT                                                             
*                                                                               
DLHNDPT  DS    0H                                                               
         MVI   0(R3),NDPTS                                                      
         B     EXIT                                                             
*                                                                               
DLHDEML  DS    0H                  LIST OF DEMO NAMES                           
         LA    RF,DNAMES+6                                                      
         LR    RE,R3                                                            
         L     R0,NADEMS                                                        
         BCT   R0,*+8              SKIP HH                                      
         BZ    EXIT                                                             
*                                                                               
DLHDL2   DS    0H                                                               
         MVC   0(5,RE),1(RF)                                                    
         LA    RE,5(RE)                                                         
         LA    RF,6(RF)                                                         
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
         BNE   DH499X                                                           
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
*                                                                               
*        BUILD MARKET DATA TABLE                                                
         SPACE 2                                                                
BLDMTAB  NTR1                                                                   
         MVC   MKTSAV+40(8),=C'ADILIST '                                        
         CLI   SRCE,C'A'                                                        
         BE    *+10                                                             
         MVC   MKTSAV+40(8),=C'DMALIST '                                        
*                                                                               
         OPEN  (MKTSAV,(INPUT))                                                 
*                                  SET BINSRCH PARS                             
         SR    R0,R0                                                            
         L     R1,=A(MKTTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,MKLEN                                                         
         LA    R4,MKKLEN                                                        
         LH    R5,=Y(MKTMAX)                                                    
         STM   R0,R5,MKTPARS                                                    
*                                                                               
         LA    RE,MW                                                            
         LA    RF,MKLEN                                                         
         XCEF                                                                   
         LA    R6,MW                                                            
         USING MKTTABD,R6                                                       
         MVC   MKNAM,=CL30'**U.S. TOTALS**'                                     
         GOTO1 BINSRCH,MKTPARS,(1,MW)                                           
*                                                                               
         L     R5,ADBUY                                                         
         USING MSRECD,R5           MARKET SAVE RECORD                           
*                                                                               
BMK4     DS    0H                                                               
         GET   MKTSAV,(R5)                                                      
         CLI   0(R5),C'*'          SKIP COMMENTS                                
         BE    BMK4                                                             
         LA    RF,MSRNUM                                                        
         PACK  DUB,0(3,RF)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,MKNUM                                                       
         MVC   MKNAM,MSRNAM                                                     
         MVC   MKTZ,MSRTZ                                                       
         MVC   MKCLS,MSRCLS                                                     
         MVC   MKREG,MSRREG                                                     
         MVC   MKABBR,MSRABBR                                                   
*                                                                               
         GOTO1 BINSRCH,MKTPARS,(1,MW)                                           
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     BMK4                NEXT MARKET                                  
*                                                                               
BMEOF    DS    0H                                                               
         L     R6,AMKTTAB                                                       
         LA    RF,MKLEN            SET EOL                                      
         MH    RF,MKTCNT+2                                                      
         AR    RF,R6                                                            
         MVI   0(RF),X'FF'                                                      
*                                                                               
         CLOSE MKTSAV                                                           
         B     EXIT                                                             
         SPACE 2                                                                
MSRECD   DSECT                                                                  
MSRNAM   DS    CL30                                                             
MSRNUM   DS    CL3                                                              
MSRTZ    DS    CL1                                                              
MSRCLS   DS    CL1                                                              
MSRREG   DS    CL2                                                              
MSRABBR  DS    CL8                                                              
         DS    CL32                SPARE                                        
         SPACE 2                                                                
SPPX02   CSECT                                                                  
MKTSAV   DCB   DDNAME=MKTSAV,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=BMEOF                                                      
*                                                                               
         SPACE 2                                                                
* SAVE UNIVERSES IN RANK TABLE                                                  
SVUNIV   NTR1                                                                   
         LA    R3,DEMLST           CHANGE DEMLST TO UNIVS                       
         LA    RF,NDEMS                                                         
*                                                                               
         MVI   1(R3),C'L'                                                       
         LA    R3,3(R3)                                                         
         BCT   RF,*-8                                                           
*                                                                               
         L     R4,ADBLOCK                                                       
         L     RF,ACOMFACS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMLST),ADBLOCK,DOUTS                            
         USING MKTTABD,R6                                                       
         MVC   MKUNVS,DOUTS                                                     
*                                                                               
         CLI   SRCE,C'N'           FOR ACN                                      
         BNE   SVU6                MUST DIVIDE BY 100                           
*                                                                               
         LA    R3,MKUNVS                                                        
         LA    R5,NDEMS                                                         
*                                                                               
SVU4     DS    0H                                                               
         L     R1,0(R3)                                                         
         SR    R0,R0                                                            
         L     RF,=F'100'                                                       
         BAS   RE,DIV                                                           
         ST    R1,0(R3)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,SVU4                                                          
*                                                                               
SVU6     DS    0H                                                               
         DROP  R6                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        GENSUBS- GENERAL SUBROUTINES AND DATA AREAS                            
         SPACE 2                                                                
GENSUBS  DS    0D                                                               
         SPACE 3                                                                
DIV      DIV   (R0),(RF)       RETURNS VIA RE                                   
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
*        DEMO LIST                                                              
*                                                                               
DEMLST   DS    0X                                                               
         DC    X'00',C'H',AL1(000)                                              
         DC    X'00',C'H',AL1(000)                                              
         DC    X'00',C'H',AL1(000)                                              
         DC    X'00',C'H',AL1(000)                                              
         DC    X'00',C'H',AL1(000)                                              
         DC    X'00',C'H',AL1(000)                                              
         DC    X'00',C'H',AL1(000)                                              
*                                                                               
NDEMS    EQU   (*-DEMLST)/3                                                     
*                                                                               
         DC    3X'FF'                                                           
*                                                                               
         SPACE 3                                                                
*        DAYPART NAME LIST                                                      
*                                                                               
DPTLST   DS    0X                                                               
         DC    CL2'PR'                                                          
         DC    CL2'EM'                                                          
         DC    CL2'DA'                                                          
         DC    CL2'EF'                                                          
         DC    CL2'EN'                                                          
         DC    CL2'PA'                                                          
         DC    CL2'LN'                                                          
         DC    CL2'LF'                                                          
         DC    CL2'SP'                                                          
         DC    CL2'CH'                                                          
NDPTS    EQU   (*-DPTLST)/2                                                     
*                                                                               
         DC    3X'FF'                                                           
*                                                                               
         SPACE 3                                                                
*        DAYPART EQUIVALENCE LIST                                               
*        ***THIS IS A TEMPORARY LIST FOR TESTING                                
*        ***FOR USE IF MEDMPDPT IS NOT SET                                      
*                                                                               
DPTEQL   DS    0X                                                               
         DC    C'P',AL1(1)         PRIME                                        
         DC    C'M',AL1(2)         EM                                           
         DC    C'D',AL1(3)         DAY                                          
         DC    C'E',AL1(4)         EFR                                          
         DC    C'N',AL1(5)         EARLY NEWS**                                 
         DC    C'A',AL1(6)         ACCESS                                       
         DC    C'*',AL1(7)         LATE NEWS**                                  
         DC    C'L',AL1(8)         LFR                                          
         DC    C'S',AL1(9)         SPT                                          
         DC    C'K',AL1(10)        KID                                          
*                                                                               
         DC    X'FFFF'                                                          
*                                                                               
NQTRS    EQU   1                   **ONLY 'TOTAL' QUATER**                      
         SPACE 3                                                                
RELO     DC    F'0'                                                             
DOUTS    DS    XL(NDEMS*4)                                                      
RSMKT    DS    H                                                                
SVADMKT  DS    A                                                                
VGLAREA  DS    A                                                                
AMKTCNT  DS    F                                                                
X        DS    XL256                                                            
SRCE     DS    C                                                                
TYPRET   DS    C                                                                
         DS    0D                                                               
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
MKTPARS  DS    6F                                                               
AMKTTAB  EQU   MKTPARS+4                                                        
MKTCNT   EQU   MKTPARS+8                                                        
*                                                                               
DPGFILE  DC    CL8'SPPX05  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
XFF      DC    20X'FF'                                                          
*                                                                               
SVQCLT   DC    XL3'00'                                                          
SVQPRD   DC    XL3'00'                                                          
SVQEST   DC    XL6'00'                                                          
SVQSTRT  DC    XL6'00'                                                          
SVQEND   DC    XL6'00'                                                          
SVQSRCE  DC    XL1'00'                                                          
SVQMENU  DC    XL4'00'                                                          
SVQQTR   DC    XL1'00'                                                          
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
NADEMS   DS    F                                                                
QTRTAB   DS    XL6                                                              
MKACTSW  DS    X                                                                
FLACTSW  DS    X                                                                
MW       DS    XL1024                                                           
         SPACE 2                                                                
         LTORG                                                                  
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
         EJECT                                                                  
*                                  PROC BUY ROUTINES                            
*                                                                               
         DS    0D                                                               
PROCB    NMOD1 0,**PRB                                                          
         L     RC,VGLAREA                                                       
         USING GLOBALD,RC                                                       
         OC    RSMKT,RSMKT         SKIP IF BAD RTG SERVICE MKT                  
         BZ    EXIT                                                             
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
*                                                                               
         L     RF,=A(SPHKRGS)      SAVE REGS FOR SPOTHOOK                       
         A     RF,RELO                                                          
         STM   R7,RC,0(RF)                                                      
*                                                                               
         CLI   QOPT3,C'Y'          TEST DOING ONLY 30'S                         
         BNE   PRB3                                                             
         CLI   BDSEC,30                                                         
         BNE   EXIT                                                             
*                                                                               
PRB3     DS    0H                                                               
         MVI   MEDBRAND,X'FF'      SET TO GET ALL BRANDS                        
         CLI   BPRD,0                                                           
         BE    PRB3B                                                            
         CLI   BPRD,X'FF'                                                       
         BE    PRB3B                                                            
         MVC   MEDBRAND,BPRD       UNLESS ONE THIS TIME                         
*                                                                               
PRB3B    DS    0H                                                               
         MVI   MEDSPTLN,0          ALL LENGTHS                                  
         GOTO1 MEDGETBY,DMCB,SPWORKD,3      ** RERATED **                       
*                                                                               
         CLI   MEDMPDPT,C' '       TEST HAVE PLANNING DAYPART                   
         BNH   PRB3D               NO                                           
*                                  YES- GET NUMBER                              
         LA    R1,DPTLST                                                        
         LA    RF,NDPTS                                                         
*                                                                               
         CLC   0(2,R1),MEDMPDPT                                                 
         BE    *+14                                                             
         LA    R1,2(R1)                                                         
         BCT   RF,*-14                                                          
         DC    H'0'                BAD PLANNING DAYPART                         
*                                                                               
         LCR   RF,RF                                                            
         LA    RF,NDPTS+1(RF)                                                   
         STC   RF,DPTNO            DAYPART NUMBER                               
         B     PRB6D                                                            
*                                  DO NOT HAVE PLANNING DAYPART                 
PRB3D    DS    0H                  **FOR TESTING- SET FROM FIXED LIST           
         CLI   QOPT5,C'T'          TEST OK TO USE LIST                          
         BNE   PRB5                NO, ERROR                                    
         LA    R1,DPTEQL                                                        
*                                                                               
PRB4     DS    0H                                                               
         CLI   0(R1),X'FF'                                                      
         BE    PRB5                                                             
         CLC   BDDAYPT,0(R1)                                                    
         BE    PRB6                                                             
         LA    R1,2(R1)                                                         
         B     PRB4                                                             
*                                                                               
PRB5     DS    0H                                                               
         TM    GLDOWNLD,X'80'      SKIP MESSAGE IF DOWNLOADING                  
         BNZ   PRB5D                                                            
         MVC   P(17),=C'** BAD DAYPART **'                                      
         MVC   P+18(1),BDDAYPT                                                  
         MVC   P+21(3),CLT                                                      
         MVC   P+25(3),PRD                                                      
         EDIT  (B1,BUYKEST),(3,P+29),FILL=0                                     
         MVC   P+33(7),STAPRINT                                                 
         EDIT  (B1,BUYKBUY),(3,P+41),FILL=0                                     
         GOTO1 REPORT                                                           
*                                                                               
PRB5D    DS    0H                                                               
         B     PRBX                SKIP BUY                                     
*                                                                               
PRB6     DS    0H                                                               
         MVC   DPTNO,1(R1)         SET DAYPART NUMBER                           
*                                                                               
PRB6D    DS    0H                                                               
         LA    R5,MEDQRT01         FIRST QTR                                    
         LA    R4,1                                                             
*                                                                               
PRB8     DS    0H                                                               
         CH    R4,=Y(NQTRS-1)                                                   
         BH    PRB9                                                             
         STC   R4,QTRNO                                                         
         BAS   RE,SETMDP           SET TO MKT/DPT/QTR TAB                       
         LA    R5,12(R5)           NEXT QTR                                     
         LA    R4,1(R4)                                                         
         B     PRB8                                                             
*                                                                               
PRB9     DS    0H                                                               
         LA    R5,MEDPERD          TOTAL                                        
         MVI   QTRNO,0                                                          
         BAS   RE,SETMDP                                                        
*                                                                               
PRBX     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*        SET MKT/DPT/QTR DATA                                                   
         SPACE 2                                                                
SETMDP   NTR1                                                                   
         L     R5,4(R5)            POINT TO DATA                                
         LTR   R5,R5                                                            
         BZ    SETMDPX                                                          
         USING MEDDATA,R5                                                       
         OC    MEDBYSPT,MEDBYSPT   TEST ANY SPOTS                               
         BZ    SETMDPX             NO - DONE                                    
         OC    MEDBYD,MEDBYD       AND DOLLARS                                  
         BZ    SETMDPX             NO - DONE                                    
         MVI   FLACTSW,C'Y'        SET HAVE ACTIVITY FOR DL FILE                
*                                                                               
         ZIC   RF,DPTNO            DISPL = DPTNO                                
         BCTR  RF,R0                                                            
         MH    RF,=Y(NQTRS)                X MAX QTRS                           
         ZIC   RE,QTRNO                    + QTRNO                              
         AR    RF,RE                                                            
         MH    RF,=Y(MKDPDL)               X ENTRY LENGTH                       
         L     R6,SVADMKT                                                       
         LA    R6,MKDPQW-MKTTABD(R6)                                            
         AR    R6,RF               POINT TO RIGHT ENTRY                         
         USING MKDPD,R6                                                         
*                                                                               
         L     R0,MKDPSPT                                                       
         A     R0,MEDBYSPT     SPOTS                                            
         ST    R0,MKDPSPT                                                       
*                                                                               
         L     R1,MEDBYDEQ     $.00 - ***EQUIVALENCED                           
         M     R0,=F'1'                                                         
         LA    RF,100                                                           
         BAS   RE,DIV                                                           
         A     R1,MKDPCST                                                       
         ST    R1,MKDPCST                                                       
*                                                                               
         L     R0,MKDPIMP                                                       
         A     R0,MEDBY1       HH IMPS - UNEQUIVALENCED                         
         ST    R0,MKDPIMP                                                       
*                              RATINGS ARE FURTHER DOWN LIST                    
         L     R2,NADEMS                                                        
         LR    R3,R2                                                            
         SLL   R2,3                8 BYTES PER DEMO                             
         LA    R2,MEDBY1(R2)                                                    
         LA    R1,MKDPGRPS                                                      
*                                                                               
SETMDP4  DS    0H                                                               
         L     R0,0(R1)                                                         
         A     R0,0(R2)                                                         
         ST    R0,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,8(R2)                                                         
         BCT   R3,SETMDP4                                                       
*                                                                               
         L     RF,SVADMKT                                                       
         MVC   MKDPUNVS,MKUNVS-MKTTABD(RF)  SET POPS                            
*                                                                               
SETMDPX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         DROP  R6                                                               
         SPACE 3                                                                
         USING *,RF                                                             
SPHOOK   NTR1                                                                   
         LM    R7,RC,SPHKRGS                                                    
         B     SPH2                                                             
*                                                                               
SPHKRGS  DS    6F                                                               
         DROP  RF                                                               
*                                                                               
SPH2     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
MKTTABD  DSECT                                                                  
MKNUM    DS    XL2                                                              
MKKLEN   EQU   *-MKTTABD                                                        
MKRNK    DS    XL2                                                              
MKNAM    DS    CL30                                                             
MKTZ     DS    CL1                                                              
MKCLS    DS    CL1                                                              
MKREG    DS    CL2                                                              
MKABBR   DS    CL8                                                              
         DS    0F                                                               
MKUNVS   DS    XL(4*NDEMS)                                                      
MKDPQW   DS    XL(MKDPDL*NDPTS*NQTRS)                                           
         DS    0F                                                               
MKLEN    EQU   *-MKTTABD                                                        
*                                                                               
MKTMAX   EQU   250                                                              
         SPACE 2                                                                
*                                                                               
MKDPD    DSECT                     COVERS MKT/DAYPART AREA                      
MKDPSPT  DS    F                   SPOTS                                        
MKDPCST  DS    F                   COST                                         
MKDPIMP  DS    F                   HH IMPS                                      
MKDPGRPS DS    XL(4*NDEMS)         GRPS - HH FIRST                              
MKDPUNVS DS    XL(4*NDEMS)         UNIVERSES                                    
MKDPDL   EQU   *-MKDPD                                                          
*                                                                               
SPPX02   CSECT                                                                  
         DS    0D                                                               
DNAMES   DS    CL(NDEMS*6*2)                                                    
         DS    0D                                                               
         DC    CL8'*MKTTAB*'                                                    
MKTTAB   DS    0X                                                               
         ORG   *+MKTMAX*MKLEN                                                   
         DS    0D                                                               
         DC    CL8'DRGLOBAL'                                                    
GLAREA   DS    40000X              DRIVER GLOBAL AREA                           
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         SPACE 2                                                                
       ++INCLUDE SPDRVWRKD                                                      
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
       ++INCLUDE SPREPPTBUF                                                     
         SPACE 2                                                                
       ++INCLUDE SPMEDBLOCK                                                     
         SPACE 2                                                                
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091SPREPPX02 05/01/02'                                      
         END                                                                    
