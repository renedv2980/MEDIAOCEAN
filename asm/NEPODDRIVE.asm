*          DATA SET NEPODDRIVE AT LEVEL 102 AS OF 09/17/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00A53B                                                                  
*INCLUDE EDITOR                                                                 
         TITLE 'T00A53 PDDRIVER - SYSTEM DRIVER FOR RESEARCH WRITER'            
PDDRIVER RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*PODDRI*,R6,R7,R8                                              
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         USING PODBD,PODBOOK                                                    
*                                                                               
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    INIT                                                             
*                                                                               
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
*                                                                               
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
*                                                                               
         CLI   GLHOOK,GLINCOMP     INTERNAL COMPUTE TIME                        
         BE    EXEC                                                             
*                                                                               
         CLI   GLHOOK,GLPUTSRT     PUT SORT RECORD                              
         BE    PUTSRT                                                           
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         J     XIT                                                              
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     B     XIT                                                              
         EJECT                                                                  
* RESOLVE ROUTINE ADDRESSES                                                     
*                                                                               
RESOLVE  L     R1,=A(ROUTLIST)                                                  
*                                                                               
RES02    CLI   0(R1),FF                                                         
         BE    RESX                                                             
         CLC   0(8,R1),GLLABEL                                                  
         BE    RES04                                                            
         LA    R1,12(R1)                                                        
         B     RES02                                                            
*                                                                               
RES04    MVC   GLAROUT,8(R1)       RETURN ADDRESS                               
         TM    12(R1),DIBYDEM+DIDEMNDX    TEST BUY DEMO INPUT LABEL             
         BZ    RES06                                                            
         CLC   NDEMOS,GLARGS+1     YES - GLARGS+1 = DEMO NUMBER                 
         BNL   RES06                                                            
         MVC   NDEMOS,GLARGS+1     KEEP TRACK OF MAX DEMOS                      
*                                                                               
RES06    OC    DATAIND,12(R1)                                                   
         OC    DATAIND2,13(R1)                                                  
         OC    DATAIND3,14(R1)                                                  
         OC    DATAIND4,15(R1)                                                  
*                                                                               
RESX     B     XIT                                                              
         EJECT                                                                  
* EXECUTING ROUTINES                                                            
*                                                                               
EXEC     MVC   WORK,BLANKS         PRESET WORK AREAS                            
         ZAP   DUB,=P'0'                                                        
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLOUTPUT                                                  
         BE    EXE02                                                            
*                                  ** INPUT ROUTINE **                          
         L     R1,GLADTENT         ADDRESS INPUT FIELD                          
         USING DRIND,R1                                                         
         MVC   INLEVEL,DRINLEV     SAVE LEVEL                                   
         B     EXEX                                                             
         DROP  R1                                                               
*                                  ** OUTPUT ROUTINE **                         
EXE02    MVC   OUTAREA,BLANKS      PRESET SOME FIELDS FOR OUTPUT                
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         MVC   OUTLEVEL,DROLEV     SAVE LEVEL                                   
         MVC   MYPOSO,DROPOS                                                    
         MVC   MYOLEN,DROLEN                                                    
         DROP  R1                                                               
*                                                                               
EXEX     L     RF,GLAROUT       ** BRANCH TO I/O ROUTINE **                     
         BR    RF                                                               
         EJECT                                                                  
* INPUT ROUTINES                                                                
ITEXT    MVI   0(R2),C' '          TEXT                                         
         B     XIT                                                              
*                                                                               
*----- PROGRAM                                                                  
INPG     BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         MVC   0(6,R2),PDCODE      PROGRAM CODE                                 
         CLI   GLARGS,C'C'                                                      
         BE    INX                                                              
         CLI   GLARGS,C'S'         SHORT PROGRAM NAME                           
         BNE   INPG02                                                           
         GOTO1 DEFINE,DMCB,=C'PROG06 ',DBLOCKA,PDSPRO                           
         MVC   0(6,R2),PDSPRO                                                   
         B     INX                                                              
*                                                                               
INPG02   CLI   GLARGS,C'L'                                                      
         BNE   INPG06                                                           
         CLC   PDFILE,=C'EVN'                                                   
         BE    INPG06                                                           
         CLI   PDLPRO+1,0          MIGHT HAVE IT ALREADY                        
         BNE   INPG04               CAREFUL IT COULD HAVE BEEN PNAMED           
         GOTO1 DEFINE,DMCB,=C'PROG25 ',DBLOCKA,PDLPRO                           
*                                                                               
INPG04   MVC   0(25,R2),PDLPRO     LONG PROGRAM NAME                            
         B     INX                                                              
*                                                                               
INPG06   CLI   GLARGS,C'B'         BOTH CODE AND PROGRAM                        
         BNE   *+8                                                              
         LA    R2,6(R2)                                                         
         MVC   0(17,R2),PDPROG                                                  
         OC    0(17,R2),=17X'40'  CHANGE ZEROS TO BLANKS                        
         B     INX                                                              
*                                                                               
*-----PROGRAM GROUP                                                             
IPRGG    MVC   0(8,R2),PDPRGCOD    PROGRAM GROUP CODE                           
         B     INX                                                              
*                                                                               
*----- PROGRAM NAME FROM GROUP                                                  
IPGN     MVC   0(16,R2),PDGPROG    ONLY ONE LENGTH FOR THIS                     
         OC    0(16,R2),=16X'40'                                                
         B     INX                                                              
*                                                                               
*-----EPISODE                                                                   
IEPI     DS    0H                                                               
         CLC   =CL3'COM',PDSOURCE  COMSCORE?                                    
         JE    *+14                                                             
         OC    PDCSSTA,PDCSSTA                                                  
         JZ    IEPI10                                                           
         MVC   0(16,R2),PDEPIS                                                  
         CLI   GLARGS,C'L'                                                      
         BNE   INX                                                              
         MVC   0(25,R2),PDLEPI                                                  
         J     INX                                                              
*                                                                               
IEPI10   GOTO1 DEFINE,DMCB,=C'EPISODE',DBLOCKA,PDEPIS                           
         MVC   0(16,R2),PDEPIS     EPISODE NAME                                 
         CLI   GLARGS,C'L'                                                      
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'EPIS25',DBLOCKA,PDLEPI                            
         MVC   0(25,R2),PDLEPI     LONG EPISODE NAME                            
         B     INX                                                              
*                                                                               
*-----EPISODE (DDS ONLY) - USED FOR ITN EPISODE                                 
IDDSEPI  DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'EPISDD',DBLOCKA,WORK                              
         MVC   0(25,R2),WORK                                                    
         B     INX                                                              
*                                                                               
*-----TRACK                                                                     
ITRAK    DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'TRAK  ',DBLOCKA,PDTRAK                            
         MVC   0(16,R2),PDTRAK     TRACK NAME                                   
         CLI   GLARGS,C'L'                                                      
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'TRAK25 ',DBLOCKA,PDLTRAK                          
         MVC   0(25,R2),PDLTRAK    LONG TRACK NAME                              
         B     INX                                                              
*                                                                               
*-----CPROG                                                                     
ICPROG   MVC   0(16,R2),PDCPROG    CABLE PROG NAME                              
         CLI   GLARGS,C'L'                                                      
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'CPROG25 ',DBLOCKA,PDLCPROG                        
         MVC   0(40,R2),PDLCPROG   LONG CABLE PROGRAM NAME                      
         B     INX                                                              
*                                                                               
*-----TRACK                                                                     
ITRNUM   MVC   0(05,R2),PDTRNUM    TRACK NUMBER                                 
         B     INX                                                              
*          DATA SET NEPODDRIVE AT LEVEL 021 AS OF 08/28/00                      
*-----TELECAST NUMBER                                                           
ITLNUM   MVC   0(05,R2),PDTLNUM    TCAST NUMBER                                 
         B     INX                                                              
*                                                                               
*-----DAILY PROGRAM NAME                                                        
IDPRG    DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'DPROG ',DBLOCKA,PDDPRO                            
         MVC   0(25,R2),PDDPRO    DAILY PROGRAM NAME                            
         B     INX                                                              
*                                                                               
*-----FILTER                                                                    
IFIL     DS    0H                                                               
         CLC   =C'PIV',PDSOURCE                                                 
         BE    IFIL02                                                           
         GOTO1 DEFINE,DMCB,=C'TYPE ',DBLOCKA,PDFILT                             
*                                                                               
IFIL02   MVC   0(4,R2),PDFILT     FILTER                                        
         B     INX                                                              
*                                                                               
*-----VIEWING TYPE                                                              
IVIEWT   DS    0H                                                               
         CLC   =CL3'COM',PDSOURCE  COMSCORE?                                    
         JE    IVIEWT6                                                          
         OC    PDCSSTA,PDCSSTA                                                  
         JNZ   IVIEWT6                                                          
*                                                                               
         GOTO1 DEFINE,DMCB,=C'NLIV ',DBLOCKA,WORK                               
         MVC   0(10,R2),WORK       VIEWING TYPE(LIVE,LIVE+SD,...)               
         LA    RF,SRTVTYP                                                       
IVIEWT2  CLI   0(RF),X'FF'                                                      
         BE    IVIEWTX                                                          
         CLC   0(1,RF),0(R2)                                                    
         BE    IVIEWT4                                                          
         LA    RF,2(RF)                                                         
         B     IVIEWT2                                                          
IVIEWT4  MVC   0(1,R2),1(RF)       REPLACE WITH SEQUENCE NUMBER                 
         J     IVIEWTX                                                          
*                                                                               
IVIEWT6  L     RE,ADBCSREX         A(CSRN EXTEND BLOCK)                         
         USING DBCSRND,RE                                                       
         CLC   DBCSRVT,=C'RL'      LIVE?                                        
         JNE   *+10                                                             
         MVC   0(10,R2),=CL10' CSLIVE'                                          
         CLC   DBCSRVT,=C'RC'      LIVE COMMERCIAL?                             
         JNE   *+10                                                             
         MVC   0(10,R2),=CL10' CSLIVECOM'                                       
         CLC   DBCSRVT,=C'R3'      PAV LIVE+3 COMMERCIAL                        
         JNE   *+10                                                             
         MVC   0(10,R2),=CL10' CSL+3COM'                                        
         CLC   DBCSRVT,=C'R7'      PAV LIVE+7 COMMERCIAL                        
         JNE   *+10                                                             
         MVC   0(10,R2),=CL10' CSL+7COM'                                        
         DROP  RE                                                               
*                                                                               
IVIEWTX  B     INX                                                              
*                                                                               
*-----BREAKOUT INDICATOR                                                        
IBKOUT   GOTO1 DEFINE,DMCB,=C'BKOUT',DBLOCKA,DUB                                
         MVC   0(1,R2),DUB                                                      
         B     INX                                                              
*                                                                               
*-----AIRING TYPE                                                               
IAIRT    GOTO1 DEFINE,DMCB,=C'TYPE ',DBLOCKA,DUB                                
         MVC   0(1,R2),DUB+3                                                    
         B     INX                                                              
*                                                                               
*-----CONTENT TYPE                                                              
ICONT    GOTO1 DEFINE,DMCB,=C'TYPE ',DBLOCKA,DUB                                
         MVC   0(1,R2),DUB+2                                                    
         B     INX                                                              
*                                                                               
*-----WEEK                                                                      
IWEK     MVC   0(7,R2),PDWEEK     WEEK                                          
         B     INX                                                              
*                                                                               
*-----NTI CODE                                                                  
INTI     MVC   0(5,R2),PDNTI      NTI                                           
         B     INX                                                              
*                                                                               
*-----LNTI CODE                                                                 
ILNTI    MVC   0(4,R2),PDNET      NTI                                           
         CLI   3(R2),C' '                                                       
         BH    *+14                                                             
         MVC   3(5,R2),PDNTI                                                    
         B     INX                                                              
         MVC   4(5,R2),PDNTI                                                    
         B     INX                                                              
*                                                                               
*-----TP/NSI NETWORK AFFILITATES                                                
IAFFIL   DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'AFFL ',DBLOCKA,PDTPAFFL                           
         MVC   0(5,R2),PDTPAFFL   NETWORK AFFILIATES                            
         B     INX                                                              
*                                                                               
*-----TP/NSI USER AFFILITATES                                                   
IUFFIL   DS    0H                                                               
         MVC   0(5,R2),PDUAFFL   USER AFFILIATES                                
         B     INX                                                              
*                                                                               
*-----COVERAGE                                                                  
ICOVER   DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'COVERAGE ',DBLOCKA,PDCOVER                        
         SR    R1,R1                                                            
         ICM   R1,3,PDCOVER                                                     
         CVD   R1,0(R2)                                                         
         ZAP   8(8,R2),=PL1'1'     SET WEIGHT                                   
         B     INX                                                              
*                                                                               
*-----STATION COUNT                                                             
ISCOUNT  DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'SCOUNT ',DBLOCKA,PDSCOUNT                         
         SR    R1,R1                                                            
         ICM   R1,3,PDSCOUNT                                                    
         CVD   R1,0(R2)                                                         
         ZAP   8(8,R2),=PL1'1'     SET WEIGHT                                   
         B     INX                                                              
*                                                                               
*-----DAY                                                                       
IDAY     MVC   0(5,R2),PDDAY      DAY                                           
*                                                                               
         CLC   =C'CTP',PDFILE     NOT ACTIVE FOR COUNTY COVERAGE                
         BNE   *+10                                                             
         MVC   0(5,R2),BLANKS     ENSURE SAME SORT ORDER                        
*                                                                               
         B     INX                                                              
*                                                                               
*-----ACTIVE DAYS                                                               
IADAY    GOTO1 DEFINE,DMCB,=C'DAY ',DBLOCKA,DUB                                 
         MVC   0(2,R2),DUB                                                      
         B     INX                                                              
*                                                                               
*-----TIME                                                                      
ITIM     MVC   0(4,R2),PDTIME    TIME                                           
*                                                                               
         CLC   =C'ITN S',PDNET    NOT ACTIVE FOR ITN                            
         BE    *+10                                                             
         CLC   =C'CTP',PDFILE     NOT ACTIVE FOR COUNTY COVERAGE                
         BNE   *+14                                                             
         MVC   0(4,R2),XFF        ENSURE SAME SORT ORDER                        
         B     ITIMX                                                            
*                                                                               
         CLC   0(2,R2),=H'559'   ADJUST FOR 6AM START                           
         BH    ITIM02                                                           
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
ITIM02   DS    0C                                                               
         CLC   2(2,R2),=H'559'   ADJUST FOR 6AM START                           
         BH    ITIMX                                                            
         SR    RE,RE                                                            
         ICM   RE,3,2(R2)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,2(R2)                                                       
*                                                                               
ITIMX    B     INX                                                              
*                                                                               
*-----END TIME (COMSCORE)                                                       
IETIME   MVC   0(2,R2),PDTIME+2    END TIME                                     
         CLC   0(2,R2),=H'559'     ADJUST FOR 6AM START                         
         BH    IETIMEX                                                          
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
IETIMEX  B     INX                                                              
*                                                                               
*-----START TIME (COMSCORE)                                                     
ISTIME   MVC   0(2,R2),PDTIME      START TIME                                   
         CLC   0(2,R2),=H'559'     ADJUST FOR 6AM START                         
         BH    ISTIMEX                                                          
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
ISTIMEX  B     INX                                                              
*                                                                               
*-----MILITARY START TIME                                                       
IMILS    DS    0H                                                               
*                                                                               
         CLC   =C'CTP',PDFILE     NOT ACTIVE FOR COUNTY COVERAGE                
         BNE   *+14                                                             
         MVC   0(4,R2),BLANKS     ENSURE SAME SORT ORDER                        
         B     IMILSX                                                           
*                                                                               
         GOTO1 DEFINE,DMCB,=C'TIME  ',DBLOCKA,DUB                               
         SR    RE,RE                                                            
         ICM   RE,3,DUB+2                                                       
         CHI   RE,2400                                                          
         BNH   *+8                                                              
         SHI   RE,2400                                                          
         EDIT  (RE),(4,0(R2)),ZERO=NOBLANK,FILL=0                               
IMILSX   B     INX                                                              
*                                                                               
*-----MILITARY END TIME                                                         
IMILE    DS    0H                                                               
*                                                                               
         CLC   =C'CTP',PDFILE     NOT ACTIVE FOR COUNTY COVERAGE                
         BNE   *+14                                                             
         MVC   0(4,R2),BLANKS     ENSURE SAME SORT ORDER                        
         B     IMILEX                                                           
*                                                                               
         GOTO1 DEFINE,DMCB,=C'TIME  ',DBLOCKA,DUB                               
         SR    RE,RE                                                            
         ICM   RE,3,DUB+4                                                       
         CHI   RE,2400                                                          
         BNH   *+8                                                              
         SHI   RE,2400                                                          
         EDIT  (RE),(4,0(R2)),ZERO=NOBLANK,FILL=0                               
IMILEX   B     INX                                                              
*                                                                               
*-----ROUNDED START TIME                                                        
IRSTIM   DS    0H                                                               
*                                                                               
         CLC   =C'CTP',PDFILE     NOT ACTIVE FOR COUNTY COVERAGE                
         BNE   *+14                                                             
         MVC   0(2,R2),BLANKS     ENSURE SAME SORT ORDER                        
         B     IRSTIMX                                                          
*                                                                               
         GOTO1 DEFINE,DMCB,=C'TIMERD',DBLOCKA,DUB                               
         MVC   0(2,R2),DUB+2                                                    
IRSTIMX  B     INX                                                              
*                                                                               
*                                                                               
*-----COMSCORE NETWORK                                                          
ICNET    MVC   0(L'PDCSSTA,R2),PDCSSTA                                          
         J     INX                                                              
*                                                                               
*-----NETWORK NUMBER (COMSCORE)                                                 
INETNUM  CLC   =C'COM',PDSOURCE    COMSCORE?                                    
         JNE   INX                                                              
         MVC   0(L'PDCSNETN,R2),PDCSNETN                                        
         B     INX                                                              
*                                                                               
*-----NETWORK                                                                   
INET     DS    0H                                                               
         OC    PDCSSTA,PDCSSTA     COMSCORE?                                    
         JZ    *+14                                                             
         MVC   0(L'PDCSSTA,R2),PDCSSTA                                          
         J     INX                                                              
*                                                                               
         MVC   0(4,R2),PDNET      NETWORK                                       
         TM    PDINSEQ,PDSEQSTA    RESEQUENCE ACTIVE                            
         BZ    INET06               NO - LET IT GO THROUGH                      
         LA    RE,0                                                             
         L     R1,APODNET                                                       
*                                                                               
INET02   CLI   0(R1),0             SCAN FOR INPUT SEQUENCE                      
         BE    INET06              NOT FOUND - LEAVE ALONE                      
         CLC   PDNET(4),0(R1)                                                   
         BE    INET04                                                           
         LA    RE,1(RE)                                                         
         LA    R1,PODNETL(R1)                                                   
         B     INET02                                                           
*                                                                               
INET04   MVI   0(R2),C'S'          INDICATE STATION                             
         STC   RE,1(R2)            INPUT SEQ                                    
*                                                                               
INET06   CLI   PDBKTYP,C'A'                                                     
         BE    INX                                                              
         CLI   PDBKTYP,X'40'                                                    
         BE    INX                                                              
         CLI   PDBKTYP,0                                                        
         BE    INX                                                              
         LR    R5,R2                                                            
         LA    R5,3(R5)                                                         
         CLI   0(R5),X'40'                                                      
         BNH   *+8                                                              
         LA    R5,1(R5)                                                         
         MVC   0(3,R5),=CL3'( )'                                                
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOKTYPE TABLE)                        
         ICM   RE,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RE                                                      
INET08   CLI   0(RE),X'FF'                                                      
         BE    INX                 BOOKTYPE NOT FOUND                           
         CLC   SPBKTYPN,PDBKTYP                                                 
         BE    INET10                                                           
         AR    RE,RF               NEXT ENTRY IN TABLE                          
         B     INET08                                                           
*                                                                               
INET10   MVC   1(2,R5),SPBKTYPA    2-CHARACTER BOOKTYPE                         
         DROP  RE                                                               
         LA    RE,2(R5)            POINT TO 2ND CHARACTER                       
         CLI   2(R5),C' '          IS IT A SPACE?                               
         BE    *+8                 YES. PLACE ')' HERE                          
         LA    RE,1(RE)            NO. ADVANCE ONE MORE CHARACTER               
         MVI   0(RE),C')'                                                       
*                                                                               
         CLI   PDBKTYP,C'U'        ALLOW USER MEDIUM                            
         BNE   INX                                                              
         CLI   PDNET+4,C'T'                                                     
         BE    INX                                                              
         MVC   2(1,R5),PDNET+4                                                  
         B     INX                                                              
*                                                                               
*-----LONG NETWORK NAME                                                         
ILNET    DS    0H                                                               
         CLC   =CL3'COM',PDSOURCE  COMSCORE?                                    
         JE    *+14                                                             
         OC    PDCSSTA,PDCSSTA                                                  
         JZ    *+14                                                             
         MVC   0(L'PDCSSTAN,R2),PDCSSTAN                                        
         B     INX                                                              
*                                                                               
         MVC   0(4,R2),PDNET      NETWORK                                       
         GOTO1 DEFINE,DMCB,=C'LNET  ',DBLOCKA,(R2)                              
         B     INX                                                              
*                                                                               
*-----FILE                                                                      
IFILE    MVC   0(3,R2),PDFILE     FILE                                          
         B     INX                                                              
*                                                                               
*-----SOURCE                                                                    
ISRCE    MVC   0(5,R2),PDSOURCE   SOURCE                                        
         B     INX                                                              
*                                                                               
*-----MARKET                                                                    
IMRKT    EDIT  PDMRKT,(4,0(R2)),ZERO=BLANK     MARKET                           
         B     INX                                                              
*                                                                               
*-----MARKET NAME                                                               
IMRKTNAM MVC   0(24,R2),PDMKTNAM               MARKET NAME                      
         B     INX                                                              
*                                                                               
*-----ALPHA MARKET CODE                                                         
IMRKTALF MVC   0(3,R2),PDMKTALF                ALPHA MARKET CODE                
         MVC   3(2,R2),=C'  '      ADJUST FOR DICT LENGTH                       
         B     INX                                                              
*                                                                               
*-----PROGRAM TYPE                                                              
IPTYP    DS    0H                                                               
         CLC   PDFILE,=C'EVN'                                                   
         BNE   IPTYP5                                                           
         L     RE,APGMREC          USE PROGRAM RECOD                            
         ST    RE,DBLOCKA+DBAREC-DBLOCK                                         
         LA    RE,13(RE)           L'NPGKEY                                     
         ST    RE,DBLOCKA+DBAQUART-DBLOCK                                       
IPTYP5   GOTO1 DEFINE,DMCB,=C'PTYPE',DBLOCKA,PDPTYP                             
         MVC   0(2,R2),PDPTYP                  PROGRAM TYPE                     
         B     INX                                                              
*                                                                               
*-----PROGRAM TYPE (4 CHAR)                                                     
IPTYPL   DS    0H                                                               
         CLC   PDFILE,=C'EVN'                                                   
         BNE   IPTYPL5                                                          
         L     RE,APGMREC          USE PROGRAM RECORD                           
         ST    RE,DBLOCKA+DBAREC-DBLOCK                                         
         LA    RE,13(RE)           L'NPGKEY                                     
         ST    RE,DBLOCKA+DBAQUART-DBLOCK                                       
IPTYPL5  GOTO1 DEFINE,DMCB,=C'PTYP4',DBLOCKA,PDPTYP8                            
         MVC   0(4,R2),PDPTYP8                 PROGRAM TYPE                     
         B     INX                                                              
*                                                                               
*-----PROGRAM TYPE (SUB)                                                        
IPTYPS   DS    0H                                                               
         CLC   PDFILE,=C'EVN'                                                   
         BNE   IPTYPS5                                                          
         L     RE,APGMREC          USE PROGRAM RECORD                           
         ST    RE,DBLOCKA+DBAREC-DBLOCK                                         
         LA    RE,13(RE)           L'NPGKEY                                     
         ST    RE,DBLOCKA+DBAQUART-DBLOCK                                       
IPTYPS5  GOTO1 DEFINE,DMCB,=C'PTYP4',DBLOCKA,PDPTYP8                            
         MVC   0(4,R2),PDPTYP8+4               PROGRAM TYPE                     
         B     INX                                                              
*                                                                               
*-----PROGRAM RECORD NEW/RETURNING INDICATOR                                    
IPGNEW   DS    0H                                                               
         CLC   PDFILE,=C'EVN'                                                   
         BNE   XIT                                                              
         L     RE,APGMREC          USE PROGRAM RECORD                           
         ST    RE,DBLOCKA+DBAREC-DBLOCK                                         
         LA    RE,13(RE)           L'NPGKEY                                     
         ST    RE,DBLOCKA+DBAQUART-DBLOCK                                       
         GOTO1 DEFINE,DMCB,=C'PNEW',DBLOCKA,DUB                                 
         MVC   0(1,R2),DUB                                                      
         B     INX                                                              
*                                                                               
*-----PROGRAM RECORD TIER                                                       
IPGTIER  DS    0H                                                               
         CLC   PDFILE,=C'EVN'                                                   
         BNE   XIT                                                              
         L     RE,APGMREC          USE PROGRAM RECORD                           
         ST    RE,DBLOCKA+DBAREC-DBLOCK                                         
         LA    RE,13(RE)           L'NPGKEY                                     
         ST    RE,DBLOCKA+DBAQUART-DBLOCK                                       
         GOTO1 DEFINE,DMCB,=C'PTIER',DBLOCKA,DUB                                
         MVC   0(1,R2),DUB                                                      
         B     INX                                                              
*                                                                               
*-----PROGRAM RECORD CONTENT RATING CODE                                        
IPGRAT   DS    0H                                                               
         CLC   PDFILE,=C'EVN'                                                   
         BNE   XIT                                                              
         L     RE,APGMREC          USE PROGRAM RECORD                           
         ST    RE,DBLOCKA+DBAREC-DBLOCK                                         
         LA    RE,13(RE)           L'NPGKEY                                     
         ST    RE,DBLOCKA+DBAQUART-DBLOCK                                       
         GOTO1 DEFINE,DMCB,=C'PRAT',DBLOCKA,DUB                                 
         MVC   0(2,R2),DUB                                                      
         B     INX                                                              
*                                                                               
*-----PROGRAM SOURCE                                                            
IPSOUR   DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'PSOURCE ',DBLOCKA,PDPSOUR                         
         MVC   0(2,R2),PDPSOUR                 PROGRAMMING SOURCE               
         B     INX                                                              
*                                                                               
*-----FEED                                                                      
IFEED    DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'FEED ',DBLOCKA,FULL                               
         MVC   0(1,R2),FULL                    FEED                             
         B     INX                                                              
*                                                                               
*-----LIVE INDICATOR                                                            
ILIVE    DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'LIVE ',DBLOCKA,FULL                               
         MVC   0(1,R2),FULL                    LIVE INDICATOR                   
         B     INX                                                              
*                                                                               
*-----COMMERCIAL STATUS                                                         
ICOMM    DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'COMM ',DBLOCKA,FULL                               
         MVC   0(3,R2),FULL                    COMMERCIAL STATUS                
         B     INX                                                              
*                                                                               
*-----GAA FLAG                                                                  
IGAA     MVC   0(1,R2),PDGAA                   GAA FLAG                         
         B     INX                                                              
*                                                                               
*-----ORIGINATING STATION (NPD)                                                 
IORIGIN  MVC   0(4,R2),PDORGSTA                HOME STATION(MPA/NPD)            
         B     INX                                                              
*                                                                               
*-----NTI OVERNITE FLAG                                                         
IDAILY   CLI   PDDAYLY,C'D'                    OVERNITE FLAG                    
         BNE   INX                                                              
         MVI   0(R2),C'Y'                                                       
         B     INX                                                              
*                                                                               
*-----EPISODE NUMBER                                                            
IEPNUM   DS    0H                                                               
         CLC   =CL3'COM',PDSOURCE  COMSCORE?                                    
         JE    *+14                                                             
         OC    PDCSSTA,PDCSSTA                                                  
         JZ    INX                                                              
         MVC   0(L'PDCSEN,R2),PDCSEN                                            
         B     INX                                                              
*                                                                               
*-----IS REPEAT AIRING                                                          
IRPT     TM    PDCSFLG,PDCSFRQ                                                  
         BZ    INX                                                              
         MVI   0(R2),C'Y'                                                       
         B     INX                                                              
*                                                                               
*-----IS SEASON PREMIERE                                                        
ISPREM   TM    PDCSFLG,PDCSFPQ                                                  
         BZ    INX                                                              
         MVI   0(R2),C'Y'                                                       
         B     INX                                                              
*                                                                               
*-----IS SPECIAL AIRING                                                         
ISPEC    TM    PDCSFLG,PDCSFSQ                                                  
         BZ    INX                                                              
         MVI   0(R2),C'Y'                                                       
         B     INX                                                              
*                                                                               
*-----NTILONG                                                                   
INTILG   DS    0H                                                               
         CLC   =CL3'COM',PDSOURCE  COMSCORE?                                    
         JE    INTILG10                                                         
         OC    PDCSSTA,PDCSSTA                                                  
         JNZ   INTILG10                                                         
         GOTO1 DEFINE,DMCB,=C'NTIL  ',DBLOCKA,PDNTILG                           
INTILG10 MVC   0(10,R2),PDNTILG                LONG NTI CODE                    
         B     INX                                                              
*                                                                               
*-----PREMIERE FLAG                                                             
IPREM    MVC   0(2,R2),PDPREM                  PREMIERE FLAG                    
         B     INX                                                              
*                                                                               
*-----AVG FLAG                                                                  
IAVGQ    BRAS  RE,IAVGQR                                                        
         B     INX                                                              
*                                                                               
*-----COMPUTE                                                                   
ICOMPUTE ZAP   0(8,R2),=P'0'      DUMMY COMPUTE                                 
         B     INX                                                              
*                                                                               
*-----BOOK                                                                      
IBOOK    BRAS  RE,IBOOKR                                                        
         B     INX                                                              
*-----DEMOS                                                                     
IDEM     DS    0H                                                               
         BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         BRAS  RE,IDMS    THIS WAS BAL                                          
         B     XIT                                                              
*                                                                               
*>>>>>>IDMS WAS HERE                                                            
*                                                                               
*-----STACKED DEMOS                                                             
ISTDEM   BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         MVI   BYTE,0                                                           
         L     R4,PDADEMTB         SET UP DEMO BUFFER                           
         USING PDDMBUFF,R4                                                      
         LA    R5,PDSTADEF        STACK DEFINITION                              
         ZIC   RE,GLARGS          DEMO OFFSET                                   
         ZIC   R1,PODDMENT        NUMBER OF DEMOS IN A SERIES                   
         MH    R1,=H'4'           CALC. LENGTH BETWEEN EACH DEMO GROUP          
         LA    RF,PDDEMOS                                                       
         CLI   GLARGS,0                                                         
         BE    *+10                                                             
         AR    RF,R1                                                            
         BCT   RE,*-2                                                           
         LA    RF,4(RF)            STACKED DEMOS ARE OFFSET 1 POSITION          
         MVC   DUB(L'PDWEIGHT),PDWEIGHT                                         
*                                                                               
ISTD02   LA    RE,PDDEMOS          SETUP FOR IMP WEIGHTING                      
         LR    R1,RF                                                            
         SR    R1,RE                                                            
*                                                                               
         CLC   PDSOURCE(5),=C'ACMWB' MKT WEIGHT FOR ACMWB ONLY                  
         BNE   ISTD03                                                           
         TM    OPTFLAG1,ACMMWTQ     & ONLY IF WEIGHTED BY COM SECONDS           
         BO    ISTD03                                                           
         LA    RE,PODDMWGT                                                      
         AR    RE,R1                                                            
         MVC   PDWEIGHT,0(RE)                                                   
*                                                                               
ISTD03   A     R1,APDD2VAL                                                      
         MVC   FULL,0(R1)                                                       
*                                                                               
*                                                                               
*--CHECK TO SEE IF DEMO WERE REQUESTED IN THE STACK                             
         CLI   1(R5),0                                                          
         BE    ISTDX                                                            
*                                                                               
         TM    0(RF),X'80'         NEGATIVE INDICATES NOT IN THIS PASS          
         BZ    ISTDVAL                                                          
         ZAP   0(8,R2),=PL1'0'     SO ZAP ALL ACCUMULATORS AND EXIT             
         MVC   8(8,R2),0(R2)                                                    
         LA    R2,16(R2)                                                        
         CLI   BYTE,0              FIRST TIME?                                  
         BNE   ISTD08                                                           
         MVI   BYTE,X'FF'                                                       
         ZAP   0(8,R2),=PL1'0'     YES. CLEAR INDICATORS TOO                    
         MVC   8(8,R2),0(R2)                                                    
         LA    R2,16(R2)                                                        
         B     ISTD08              CONTINUE WITH NEXT DEMO/NAD BREAK            
*                                                                               
ISTDVAL  ICM   R1,15,0(RF)                                                      
         CVD   R1,0(R2)                                                         
         ZAP   8(8,R2),=PL1'0'     SET WEIGHT TO ZERO                           
*                                                                               
         OC    0(4,RF),0(RF)       IF DEMO = TO ZERO DONT SET WEIGHT            
         BNZ   ISTD03H             NEED WEIGHT FOR AVERAGING                    
         CLC   =C'NAD',PDSOURCE    MULTIPLE READ??  (NAD ESPECIALLY)            
         BE    ISTD03D               SKIP WEIGHT 2ND+ TIME AROUND               
         CLC   =C'NAW',PDSOURCE                                                 
         BE    ISTD03D                                                          
         CLC   =C'NHT',PDSOURCE                                                 
         BE    ISTD03D                                                          
         CLC   =C'HPM',PDSOURCE                                                 
         BE    ISTD03D                                                          
         CLC   =C'NHW',PDSOURCE                                                 
         BE    ISTD03D                                                          
         CLC   =C'HPW',PDSOURCE                                                 
         BE    ISTD03D                                                          
         OC    PDWEIGHT,PDWEIGHT   WEIGHT ALSO HAS TO BE ZERO                   
         BNZ   ISTD03H                                                          
*                                                                               
ISTD03D  CLI   PDNET+4,C'C'        AVG ZERO DEMS FOR CABLE                      
         BE    *+10                                                             
         CLC   PDFILE(2),=C'TP'    AVG ZERO DEMS FOR TP                         
         BE    *+14                                                             
         OC    0(4,RF),0(RF)       IF DEMO = TO ZERO DONT SET WEIGHT            
         BZ    ISTD04                                                           
*                                                                               
ISTD03H  ICM   R1,15,PDWEIGHT      GET THE WEIGHTING FACTOR                     
         BNZ   ISTD03K                                                          
         ZAP   0(8,R2),=PL1'0'     IF ZERO ZAP THE DEMO VALUE                   
         B     ISTD04                                                           
ISTD03K  CVD   R1,8(R2)                                                         
         ZAP   WORK(16),=PL1'0'                                                 
         MVC   WORK+8(8),0(R2)                                                  
         MP    WORK(16),8(8,R2)    MULT DEMO BY WEIGHT                          
         MVC   0(8,R2),WORK+8                                                   
         CLI   PDBASE,C'B'                                                      
         BE    *+8                                                              
         CLI   PDBASE,C'I'                                                      
         BNE   ISTD04                                                           
         ICM   R1,15,FULL          STILL NEED WEIGHT IF VALUE ZERO              
         BNZ   *+8                                                              
         LA    R1,1                                                             
         ZAP   WORK(16),=PL1'0'                                                 
         CVD   R1,WORK+8                                                        
         MP    WORK(16),8(8,R2)                                                 
         MVC   8(8,R2),WORK+8                                                   
*                                                                               
*-----SET SOURCE INDICATOR                                                      
ISTD04   LA    R2,16(R2)                                                        
         CLI   BYTE,0              CHECK FIRST PASS                             
         BNZ   ISTD08                                                           
         MVI   BYTE,X'FF'                                                       
*                                                                               
         CLI   PDMETHOD,6          FOR METHOD=AE                                
         BNE   ISTD04A                                                          
         ICM   RE,15,PDWEIGHT                                                   
         CVD   RE,0(R2)            SAVE WEIGHT AT 16(R2)                        
         LA    R2,8(R2)                                                         
         B     ISTD06                                                           
*                                                                               
ISTD04A  CLC   PDSOURCE(3),=CL3'CNA'   CABLE NAD AND MVGO (TP AND PROG)         
         BE    *+10                                                             
         CLC   PDSOURCE(3),=CL3'NAD'                                            
         BNE   *+18                                                             
         ZAP   0(8,R2),=PL1'1'                                                  
         LA    R2,8(R2)                                                         
         B     ISTD06                                                           
         CLC   PDSOURCE(3),=CL3'PIV'                                            
         BNE   *+18                                                             
         ZAP   0(8,R2),=PL1'-1'                                                 
         LA    R2,8(R2)                                                         
         B     ISTD06                                                           
         ZAP   0(8,R2),=PL1'0'                                                  
         LA    R2,8(R2)                                                         
*                                                                               
*-----SET MEDIA INDICATOR                                                       
ISTD06   CLI   PDMEDIA,C'C'                                                     
         BNE   *+18                                                             
         ZAP   0(8,R2),=PL1'1'                                                  
         LA    R2,8(R2)                                                         
         B     ISTD08                                                           
         CLI   PDMEDIA,C'S'                                                     
         BNE   *+18                                                             
         ZAP   0(8,R2),=PL1'0'                                                  
         LA    R2,8(R2)                                                         
         B     ISTD08                                                           
         ZAP   0(8,R2),=PL1'0'                                                  
         LA    R2,8(R2)                                                         
*                                                                               
ISTD08   LA    R5,2(R5)                                                         
         LA    RF,4(RF)                                                         
         B     ISTD02                                                           
*                                                                               
ISTDX    MVC   PDWEIGHT,DUB                                                     
         B     INX                                                              
         DROP  R4                                                               
*                                                                               
*-----DAYPART                                                                   
IDPT     MVC   0(7,R2),PDDP       DAYPART                                       
         B     INX                                                              
*                                                                               
*-----IN-HOUSE DAYPART NAME                                                     
IDPNAME  L     R5,=A(FILTDPTB)                                                  
         MVC   0(5,R2),=C'OTHER'  USE OTHER IF NONE FOUND                       
*                                                                               
IDPN02   ZIC   RE,1(R5)                                                         
         LA    RF,2(R5)                                                         
*                                                                               
IDPN04   MVC   BYTE,0(RF)          MOVE DAY                                     
         NC    BYTE,PDDAY+1        CHECK FOR DAY MATCH                          
         BZ    IDPN06              NO BYPASS                                    
         CLC   1(2,RF),PDTIME                                                   
         BH    IDPN06                                                           
         CLC   3(2,RF),PDTIME                                                   
         BNL   IDPN08                                                           
*                                                                               
IDPN06   LA    RF,5(RF)                                                         
         BCT   RE,IDPN04                                                        
*                                                                               
         LA    R5,17(R5)                                                        
         CLI   0(R5),X'FF'                                                      
         BE    IDPNX                                                            
         B     IDPN02                                                           
*                                                                               
IDPN08   MVC   0(5,R2),12(R5)                                                   
*                                                                               
IDPNX    B     INX                                                              
*                                                                               
*-----FIRST AIR DATE                                                            
IFDT     CLC   PDFILE(3),=CL3'NTI'                                              
         BE    IFDT02                                                           
         CLC   PDFILE(3),=CL3'RLD'                                              
         BE    IFDT02                                                           
*                                                                               
*-----PIV DATA                                                                  
         MVC   0(2,R2),PDFAIR    FIRST AIR DATE                                 
         B     INX                                                              
*                                                                               
IFDT02   GOTOR UNWEEK,DMCB,PDSBOOK,DUB                                          
*                                                                               
         ZIC   R5,PDDAY                                                         
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         LA    R5,1                                                             
         CH    R5,=H'8'                                                         
         BL    *+8                                                              
         LA    R5,1                                                             
         BCTR  R5,0                                                             
         GOTO1 ADDAY,DMCB,DUB,WORK,(R5)                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,0(R2))                                   
         B     INX                                                              
*                                                                               
*-----LAST AIR DATE                                                             
ILDT     MVC   0(2,R2),PDLAIR    LAST AIR DATE                                  
         B     INX                                                              
*                                                                               
*-----TIMES AIRED                                                               
IRUN     BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         MVC   0(4,R2),PDRUN     TIMES AIRED                                    
         B     INX                                                              
*                                                                               
*-----TOTAL DURATION                                                            
IDURTOT  BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         ICM   RE,15,PDWEIGHT                                                   
         MH    RE,PDRUN+2                                                       
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   *+10                                                             
         SR    RE,RE                                                            
         ICM   RE,15,PDMINS                                                     
         ST    RE,0(R2)                                                         
         ST    RE,4(R2)                                                         
         B     INX                                                              
*-----MONITOR PLUS DOLLARS                                                      
IMPDOLO  BRAS  RE,FILTNET          OLD MPDOL ROUTINE                            
         BNZ   XIT                                                              
         ICM   RE,15,PDMPDOL                                                    
         MH    RE,PDRUN+2                                                       
         ST    RE,0(R2)                                                         
         ST    RE,4(R2)                                                         
         B     INX                                                              
*                                                                               
*-----MONITOR PLUS DOLLARS                                                      
IMPDOL   BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         ICM   RE,15,PDMPDOL                                                    
         CVD   RE,0(R2)                                                         
*                                                                               
         ICM   R1,15,PDWEIGHT                                                   
         CVD   R1,8(R2)                                                         
         ZAP   WORK(16),=PL1'0'                                                 
         MVC   WORK+8(8),0(R2)                                                  
         MP    WORK(16),8(8,R2)    MULT BY WEGHT                                
         MVC   0(8,R2),WORK+8                                                   
         B     INX                                                              
*                                                                               
*-----TOTAL WEIGHT (TIMES AIRED * WEIGHT)                                       
IWT      BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         ICM   RE,15,PDWEIGHT                                                   
         CLC   =C'MXM ',PDSOURCE   DONT USE PDRUN FOR MXM (ZERO)                
         BE    IWT5                                                             
         OC    PDRUN,PDRUN                                                      
         BZ    *+14                                                             
         CLC   PDFILE(3),=C'NAD'                                                
         BE    *+8                                                              
         MH    RE,PDRUN+2                                                       
IWT5     ST    RE,0(R2)                                                         
         B     INX                                                              
*                                                                               
*-----DURATION                                                                  
IDUR     MVC   0(4,R2),PDMINS    DURATION                                       
         B     INX                                                              
*                                                                               
*-----RUN DATE                                                                  
IRDT     CLC   =CL3'COM',PDSOURCE  COMSCORE?                                    
         JE    IRDT3                                                            
         OC    PDCSSTA,PDCSSTA                                                  
         JNZ   IRDT3                                                            
         CLC   PDFILE(3),=CL3'NTI'                                              
         BE    IRDT2                                                            
         CLC   PDFILE(3),=CL3'RLD'                                              
         BE    IRDT2                                                            
         CLC   =CL3'NAW',PDSOURCE                                               
         BE    IRDT2                                                            
         CLC   =CL3'BBM',PDSOURCE                                               
         BE    IRDT2                                                            
         CLC   =CL3'CSI',PDSOURCE                                               
         BNE   INX                                                              
*                                                                               
IRDT2    GOTOR UNWEEK,DMCB,PDSBOOK,DUB                                          
*                                                                               
         ZIC   R4,PDDAY            COMPUTING DATE - GET DAY NO IN R2            
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LA    R4,1                M-F=MON                                      
         CH    R4,=H'8'                                                         
         BL    *+8                                                              
         LA    R4,1                M-S=MON                                      
         BCTR  R4,0                CHANGE TO 0-6                                
         GOTO1 ADDAY,DMCB,DUB,WORK,(R4)                                         
         MVC   0(6,R2),WORK                                                     
         B     INX                                                              
*                                                                               
IRDT3    GOTO1 DATCON,DMCB,(3,PDDATE),(0,0(R2))    COMSCORE                     
         B     INX                                                              
*                                                                               
*-----DEMO/TIMES AIRED                                                          
IDEMAIR  BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         CLC   =C'MXM ',PDSOURCE   N/A FOR MXM                                  
         BNE   IDEMAIR5                                                         
         ZAP   0(8,R2),=PL1'0'                                                  
         ZAP   8(8,R2),=PL1'0'                                                  
         B     XIT                                                              
IDEMAIR5 BRAS  RE,IDMS           GET DEMO INFORMATION                           
         ICM   R1,15,PDRUN                                                      
         CVD   R1,32(R2)           SET AIRED                                    
         B     INX                                                              
*                                                                               
*-----DEMO/REPEAT FLAG                                                          
IDEMRPT  BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         BRAS  RE,IDMS           GET DEMO INFORMATION                           
         SR    R1,R1               SET DEFAULT                                  
         CLI   PDFILT+3,C'R'       IS IT A REPEAT                               
         BNE   *+8                                                              
         LA    R1,1000             YES - SET REPEAT FLAG                        
         CVD   R1,32(R2)           SET AIRED                                    
         B     INX                                                              
*                                                                               
*-----STACK DATA                                                                
ISTDATA  ZAP   0(7,R2),=PL1'1'   JUST PASS ANYTHING                             
         B     INX                                                              
*                                                                               
*-----STATION CALL LETTERS                                                      
ISTAT    OC    PDCSSTA,PDCSSTA                                                  
         JZ    *+14                                                             
         MVC   0(L'PDCSSTA,R2),PDCSSTA                                          
         J     INX                                                              
*                                                                               
         MVC   0(4,R2),PDSTAT                                                   
         TM    PDINSEQ,PDSEQSTA    RESEQUENCE ACTIVE                            
         BZ    ISTA06               NO - LET IT GO THROUGH                      
         LA    RE,0                                                             
         L     R1,APODNET                                                       
*                                                                               
ISTA02   CLI   0(R1),0             SCAN FOR INPUT SEQUENCE                      
         BE    ISTA06               NOT FOUND - LEAVE ALONE                     
         CLC   PDSTAT(4),0(R1)                                                  
         BE    ISTA04                                                           
         LA    RE,1(RE)                                                         
         LA    R1,PODNETL(R1)                                                   
         B     ISTA02                                                           
*                                                                               
ISTA04   MVI   0(R2),C'S'          INDICATE STATION                             
         STC   RE,1(R2)            INPUT SEQ                                    
*                                                                               
ISTA06   CLI   GLARGS,C'S'         SHORT - JUST EXIT                            
         BE    INX                                                              
         OC    PDSPILL,PDSPILL                                                  
         BZ    ISTA08                                                           
         MVI   4(R2),C'/'                                                       
         EDIT  (2,PDSPILL),(3,5(R2))                                            
         B     INX                                                              
*                                                                               
ISTA08   CLI   PDBKTYP,0                                                        
         BE    INX                                                              
         CLI   PDBKTYP,X'40'                                                    
         BNH   INX                                                              
         LR    R5,R2                                                            
         LA    R5,3(R5)                                                         
         CLI   0(R5),X'40'                                                      
         BNH   *+8                                                              
         LA    R5,1(R5)                                                         
         MVC   0(3,R5),=CL3'( )'                                                
*                                                                               
         L     RF,ACOMFACS         DISPLAY BOOKTYPE                             
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOKTYPE TABLE)                        
         ICM   RE,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RE                                                      
ISTA10   CLI   0(RE),X'FF'                                                      
         BE    INX                 BOOKTYPE NOT FOUND                           
         CLC   SPBKTYPN,PDBKTYP                                                 
         BE    ISTA12                                                           
         AR    RE,RF                                                            
         B     ISTA10                                                           
*                                                                               
ISTA12   MVC   1(2,R5),SPBKTYPA    2-CHARACTER BOOKTYPE                         
         DROP  RE                                                               
         LA    RE,2(R5)            POINT TO 2ND CHARACTER                       
         CLI   2(R5),C' '          IS IT A SPACE?                               
         BE    *+8                 YES. PLACE ')' HERE                          
         LA    RE,1(RE)            NO. ADVANCE ONE MORE CHARACTER               
         MVI   0(RE),C')'                                                       
         B     INX                                                              
*                                                                               
*-----PURE NUMBER                                                               
IPURE    DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'PURE ',DBLOCKA,DUB                                
         MVC   PDPURENO,DUB+3                                                   
         MVC   0(4,R2),PDPURENO                                                 
         B     INX                                                              
*                                                                               
*-----QUARTER HOUR DURATION                                                     
IQHDUR   EDIT  PDQHDUR,(3,0(R2)),ZERO=BLANK                                     
         B     INX                                                              
*                                                                               
*-----SID PERIOD                                                                
IPERIOD  MVC   0(4,R2),PDSDPER                                                  
         B     INX                                                              
*                                                                               
*-----SID PROGRAM TYPE EXPANSION                                                
IPRGTYP  MVC   0(7,R2),PDPRGTYP                                                 
         B     INX                                                              
*                                                                               
*-----MARKET GROUP ID                                                           
IGRPID   MVC   0(5,R2),PDQMGR                                                   
         B     INX                                                              
*                                                                               
*-----SID EFFECTIVE DATE                                                        
IEFFDAT  OC    PDEFFDAT,PDEFFDAT                                                
         BZ    INX                                                              
         CLC   PDSOURCE,=C'IUN'                                                 
         BE    IEFF02                                                           
         GOTO1 DATCON,DMCB,(3,PDEFFDAT),(4,(R2))                                
         B     INX                                                              
*                                                                               
IEFF02   GOTO1 DATCON,DMCB,(3,PDEFFDAT),(0,(R2))                                
         B     INX                                                              
*                                                                               
*-----PURE NUMBER + EFFECTIVE DATE                                              
ISIU     DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'PURE ',DBLOCKA,DUB                                
         MVC   PDPURENO,DUB+3                                                   
         MVC   0(4,R2),PDPURENO                                                 
         CLC   PDSOURCE,=C'IUN'                                                 
         BNE   INX                                                              
         GOTO1 DATCON,DMCB,(3,PDEFFDAT),(0,5(R2))                               
         B     INX                                                              
*                                                                               
*-----WEIGHT                                                                    
IWEIGHT  EDIT  PDWEIGHT,(6,0(R2)),ZERO=BLANK                                    
         B     INX                                                              
*                                                                               
*-----COST                                                                      
ICOST    SR    R1,R1                                                            
         ICM   R1,15,PDCOST                                                     
         CVD   R1,0(R2)                                                         
         B     INX                                                              
*                                                                               
*-----SQAD QUARTER                                                              
ISQART   EDIT  (B1,PDSQART+1),(2,0(R2))                                         
         MVI   2(R2),C'/'                                                       
         EDIT  (B1,PDSQART),(2,3(R2))                                           
         B     INX                                                              
*                                                                               
*-----COUNTY NAME                                                               
ICTYNAM  DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'CTYN  ',DBLOCKA,WORK                              
         MVC   0(20,R2),WORK                                                    
         B     INX                                                              
*                                                                               
*-----COUNTY NUMBER                                                             
ICTYNO   DS    0H                                                               
         XC    0(2,R2),0(R2)                                                    
         CLC   =C'CTP',PDFILE                                                   
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'CTY#  ',DBLOCKA,WORK                              
         MVC   0(2,R2),WORK                                                     
         B     INX                                                              
*                                                                               
*-----DMA NUMBER                                                                
IDMANO   DS    0H                                                               
         XC    0(2,R2),0(R2)                                                    
         CLC   =C'CTP',PDFILE                                                   
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'DMA#  ',DBLOCKA,WORK                              
         MVC   0(2,R2),WORK                                                     
         B     INX                                                              
*                                                                               
*-----DMA OF ORIGIN                                                             
IDMAORG  DS    0H                                                               
         XC    0(2,R2),0(R2)                                                    
         CLC   =C'CTP',PDFILE                                                   
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'DMAO  ',DBLOCKA,WORK                              
         MVC   0(2,R2),WORK                                                     
         B     INX                                                              
*                                                                               
*-----DMA NAME                                                                  
IDMANAM  DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'DMAN  ',DBLOCKA,WORK                              
         MVC   0(26,R2),WORK                                                    
         B     INX                                                              
*                                                                               
*-----STATE CODE                                                                
ISTATE   DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'STATE ',DBLOCKA,WORK                              
         MVC   0(2,R2),WORK                                                     
         B     INX                                                              
*                                                                               
*-----STATE NAME                                                                
ISTANAM  DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'STANM ',DBLOCKA,WORK                              
         MVC   0(25,R2),WORK                                                    
         B     INX                                                              
*                                                                               
*----- SYSCODE                                                                  
ISYSC    DS    0H                                                               
         MVC   0(2,R2),DBLOCKA+DBSELSYC-DBLOCK                                  
ISYSCX   B     INX                                                              
*                                                                               
*----- UNIVERSE YEAR                                                            
IUYEAR   DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'UYEAR',DBLOCKA,DUB                                
         MVC   0(2,R2),DUB                                                      
IYEARX   B     INX                                                              
*                                                                               
*----- AUDIENCE ESTIMATOR FLAG                                                  
IAE      DS    0H                                                               
         MVC   0(L'PDAEMETH,R2),PDAEMETH                                        
IAEX     B     INX                                                              
*                                                                               
*----- LOAD DATE                                                                
ILDDAT   DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'LDDAT',DBLOCKA,DUB                                
         MVC   0(2,R2),DUB                                                      
ILDDATX  B     INX                                                              
*                                                                               
*----- INPUT VIEWING TYPE                                                       
IIVTYP   MVI   0(R2),C' '                                                       
         ICM   RE,15,DBLOCKA+DBEXTEND-DBLOCK                                    
         BZ    IIVTYPX                                                          
         USING DBXLIVD,RE                                                       
IIVT10   CLC   DBXLIVID,=C'NLIV'                                                
         BE    IIVT20                                                           
         ICM   RE,15,DBXLIVNX                                                   
         BZ    IIVTYPX                                                          
         B     IIVT10                                                           
IIVT20   LA    RF,IVTYTAB                                                       
IIVT22   CLI   0(RF),X'FF'                                                      
         BE    IIVTYPX                                                          
         CLC   DBXLIVE,0(RF)                                                    
         BE    IIVT30                                                           
         LA    RF,2(RF)                                                         
         B     IIVT22                                                           
IIVT30   MVC   0(1,R2),1(RF)                                                    
         B     IIVTYPX                                                          
         DROP  RE                                                               
IIVTYPX  B     INX                                                              
*                                                                               
*----- COMMERCIAL DURATION                                                      
ICDUR    BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         GOTO1 DEFINE,DMCB,=C'COMS',DBLOCKA,WORK                                
         XC    0(2,R2),0(R2)                                                    
         MVC   2(2,R2),WORK        NUMBER OF COMMERCIAL SECONDS                 
*                                                                               
* FOR SYNDICATION, WE RECEIVE ONLY WEEKLY AVERAGES AND NO INDIVIDUAL            
* DAYS. ON A REQUEST FOR INDIVIDUAL DAYS (DBBEST NOT 'L'), DEMAND               
* RETURNS THE AVERAGE RECORD ONCE FOR EACH DAY IN THE AVERAGE. THE              
* COMMERCIAL DURATION PROVIDED IS FOR ALL THE DAYS IN THE AVERAGE, SO           
* WE NEED TO DIVIDE IT BY THE NUMBER OF DAYS.                                   
*                                                                               
         CLI   PDMEDIA,C'S'        FOR SYNDICATION                              
         BNE   ICDURX                                                           
         CLI   DBLOCKA+DBBEST-DBLOCK,C'L'       DBBEST=L                        
         BE    ICDURX              DON'T DIVIDE IF ONE-LINE AVERAGE             
         GOTO1 DEFINE,DMCB,=C'CNTDAYS',DBLOCKA,WORK                             
         ZIC   R0,WORK             NUMBER OF DAYS                               
         LTR   R0,R0                                                            
         BZ    ICDURX                                                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RF,15,0(R2)         TOTAL COMMERCIAL DURATION                    
         DR    RE,R0               DIVIDED BY NUMBER OF DAYS                    
         MHI   RE,2                PERFORM ROUNDING                             
         CR    RE,R0                                                            
         BL    *+8                                                              
         AHI   RF,1                                                             
         STCM  RF,15,0(R2)         COMMERCIAL DURATION FOR THE IND DAY          
*                                                                               
ICDURX   B     INX                                                              
*                                                                               
*----- NUMBER OF COMMERCIAL TELECASTS                                           
ICTCAST  BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         GOTO1 DEFINE,DMCB,=C'COMT',DBLOCKA,WORK                                
         MVC   0(1,R2),WORK        NUMBER OF COMMERCIAL TELECASTS               
*                                                                               
ICTCASTX B     INX                                                              
*                                                                               
*----- PROGRAM RECORD VIEWING SOURCE AND TYPE                                   
IPVS     DS    0H                                                               
         MVC   0(L'PDPVS,R2),PDPVS                                              
IPVSX    B     INX                                                              
*----- MARKET BREAK INFO                                                        
INADCDUR BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         XC    WORK,WORK                                                        
         MVC   WORK(1),GLARGS                                                   
         GOTO1 DEFINE,DMCB,=C'MBINF',DBLOCKA,WORK                               
         XC    0(2,R2),0(R2)                                                    
         MVC   2(2,R2),WORK        NUMBER OF COMMERCIAL SECONDS                 
INADCDUX B     INX                                                              
*                                                                               
*-----PROGRAM MINUTE                                                            
IPRMIN   DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'PRMIN',DBLOCKA,FULL                               
         MVC   0(2,R2),FULL         PROGRAM MINUTE IN HHMM FORMAT               
         MVI   2(R2),X'FF'          FORCE IT TO OUTPUT ROUTINE FOR 0            
         B     INX                                                              
*                                                                               
*-----COMMERCIAL FLAG                                                           
ICOMMF   DS    0H                                                               
         CLC   =C'MXM ',PDSOURCE   BLANK FOR SOURCES OTHER THAN MXM             
         BNE   INX                                                              
         MVI   1(R2),C' '                                                       
         MVC   0(2,R2),PDCOMFDF                                                 
         B     INX                                                              
*                                                                               
*-----PROMO FLAG                                                                
IPROMOF  DS    0H                                                               
         CLC   =C'MXM ',PDSOURCE   BLANK FOR SOURCES OTHER THAN MXM             
         BNE   INX                                                              
         CLI   PDMCOMF,C'U'        UNKNOWN COMMERCIAL CONTENT                   
         BNE   *+12                                                             
         MVI   0(R2),C'U'                                                       
         B     INX                                                              
         MVI   0(R2),C'N'                                                       
         CLI   PDMPROSC,0         CHECK ANY PROMO SECONDS                       
         BNH   *+8                                                              
         MVI   0(R2),C'Y'         YES                                           
         B     INX                                                              
*                                                                               
*-----PSA FLAG                                                                  
IPSAF    DS    0H                                                               
         CLC   =C'MXM ',PDSOURCE   BLANK FOR SOURCES OTHER THAN MXM             
         BNE   INX                                                              
         CLI   PDMCOMF,C'U'        UNKNOWN COMMERCIAL CONTENT                   
         BNE   *+12                                                             
         MVI   0(R2),C'U'                                                       
         B     INX                                                              
         MVI   0(R2),C'N'                                                       
         CLI   PDMPSASC,0         CHECK ANY PSA SECONDS                         
         BNH   *+8                                                              
         MVI   0(R2),C'Y'         YES                                           
         B     INX                                                              
*                                                                               
*-----PROGRAM+ FLAG (PROGRAM AND UNREPORTED COMMERCIALS)                        
IPGPF    DS    0H                                                               
         CLC   =C'MXM ',PDSOURCE   BLANK FOR SOURCES OTHER THAN MXM             
         BNE   INX                                                              
         CLI   PDMCOMF,C'U'        PROGRAM HAS UNKNOWN CONTENT                  
         BNE   *+12                                                             
         MVI   0(R2),C'U'                                                       
         B     INX                                                              
         MVI   0(R2),C'N'                                                       
         ZIC   RE,PDMCOMSC         COMMERCIAL SECONDS                           
         ZIC   RF,PDMPROSC         PROMO SECONDS                                
         AR    RE,RF                                                            
         ZIC   RF,PDMPSASC         PSA SECONDS                                  
         AR    RE,RF                                                            
         CHI   RE,60                                                            
         BNL   INX                 60 OR OVER. NO PROGRAM+                      
         MVI   0(R2),C'Y'          OTHERWISE IT'S PROGRAM+                      
         B     INX                                                              
*                                                                               
*-----COMMERCIAL SECONDS                                                        
ISECCOM  BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         XC    0(4,R2),0(R2)                                                    
         MVC   3(1,R2),PDMCOMSC    COMMERCIAL SECONDS                           
         B     INX                                                              
*                                                                               
*-----PROMO SECONDS                                                             
ISECPRO  BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         XC    0(4,R2),0(R2)                                                    
         MVC   3(1,R2),PDMPROSC    PROMO SECONDS                                
         B     INX                                                              
*                                                                               
*-----PSA SECONDS                                                               
ISECPSA  BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         XC    0(4,R2),0(R2)                                                    
         MVC   3(1,R2),PDMPSASC    PSA SECONDS                                  
         B     INX                                                              
*                                                                               
*-----TOTAL SECOND DURATION                                                     
ISECDUR  BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         XC    0(4,R2),0(R2)                                                    
         CLC   =C'MXM ',PDSOURCE   ONLY FOR SOURCE MXM                          
         BNE   INX                                                              
         MVI   3(R2),60            60 SECONDS IN A MINUTE                       
         B     INX                                                              
*                                                                               
*-----PPROGRAM+ SECONDS (PROGRAM AND UNREPORTED COMMERCIALS)                    
ISECPGP  BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         XC    0(4,R2),0(R2)                                                    
         CLC   =C'MXM ',PDSOURCE   ONLY FOR SOURCE MXM                          
         BNE   INX                                                              
         CLI   PDMCOMF,C'U'        PROGRAM HAS UNKNOWN CONTENT                  
         BE    INX                                                              
         ZIC   RE,PDMCOMSC         COMMERCIAL SECONDS                           
         ZIC   RF,PDMPROSC         PROMO SECONDS                                
         AR    RE,RF                                                            
         ZIC   RF,PDMPSASC         PSA SECONDS                                  
         AR    RE,RF                                                            
         CHI   RE,60                                                            
         BNL   INX                 60 OR OVER. NO PROGRAM+                      
         LHI   RF,60                                                            
         SR    RF,RE                                                            
         ST    RF,0(R2)            BALANCE IS PROGRAM+                          
         B     INX                                                              
*                                                                               
*-----TOTAL COMMERCIAL SECONDS (COMMERCIALS, PROMOS, PSAS)                      
ITSECCOM BRAS  RE,FILTNET                                                       
         BNZ   XIT                                                              
         XC    0(4,R2),0(R2)                                                    
         ZIC   RE,PDMCOMSC         COMMERCIAL SECONDS                           
         ZIC   RF,PDMPROSC         PROMO SECONDS                                
         AR    RE,RF                                                            
         ZIC   RF,PDMPSASC         PSA SECONDS                                  
         AR    RE,RF                                                            
         STCM  RE,15,0(R2)         TOTAL COMMERCIAL SECONDS                     
         B     INX                                                              
*                                                                               
*-----POD START TIME                                                            
IPDSTIM  DS    0H                                                               
         MVC   0(2,R2),PDPDSTIM     POD START TIME IN HHMM FORMAT               
         B     INX                                                              
*                                                                               
*-----POD END TIME                                                              
IPDETIM  DS    0H                                                               
         MVC   0(2,R2),PDPDETIM     POD START END IN HHMM FORMAT                
         B     INX                                                              
*                                                                               
*-----POD LENGTH IN MINUTES                                                     
IPDLEN   DS    0H                                                               
         MVC   0(2,R2),PDPDLMIN     POD LENGTH IN MINUTES                       
         B     INX                                                              
*                                                                               
*-----POD NUMBER                                                                
IPDNUM   DS    0H                                                               
         MVC   0(2,R2),PDPDNUM      POD NUMBER                                  
         B     INX                                                              
*                                                                               
*-----% COMMERCIAL SECONDS                                                      
IPCOMM   DS    0H                                                               
         XC    0(8,R2),0(R2)                                                    
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   INX                                                              
         MVC   3(1,R2),PDMCOMSC    COMMERCIAL SECONDS                           
         MVC   4(4,R2),=AL4(60)    TOTAL MINUTE IN SECONDS                      
         B     INX                                                              
*                                                                               
*-----% PROMO SECONDS                                                           
IPPROMO  DS    0H                                                               
         XC    0(8,R2),0(R2)                                                    
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   INX                                                              
         MVC   3(1,R2),PDMPROSC    PROMO SECONDS                                
         MVC   4(4,R2),=AL4(60)    TOTAL MINUTE IN SECONDS                      
         B     INX                                                              
*                                                                               
*-----% TOTAL COMMERCIAL SECONDS                                                
IPTCOMM  DS    0H                                                               
         XC    0(8,R2),0(R2)                                                    
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   INX                                                              
         ZIC   RE,PDMCOMSC         COMMERCIAL SECONDS                           
         ZIC   RF,PDMPROSC         PROMO SECONDS                                
         AR    RE,RF                                                            
         ZIC   RF,PDMPSASC         PSA SECONDS                                  
         AR    RE,RF                                                            
         STCM  RE,15,0(R2)                                                      
         MVC   4(4,R2),=AL4(60)    TOTAL MINUTE IN SECONDS                      
         B     INX                                                              
*                                                                               
*-----% PROGRAM+ SECONDS                                                        
IPPROGP  DS    0H                                                               
         XC    0(8,R2),0(R2)                                                    
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   INX                                                              
         CLI   PDMCOMF,C'U'        PROGRAM HAS UNKNOWN CONTENT                  
         BE    INX                  DON'T TRY TO COMPUTE                        
         ZIC   RE,PDMCOMSC         COMMERCIAL SECONDS                           
         ZIC   RF,PDMPROSC         PROMO SECONDS                                
         AR    RE,RF                                                            
         ZIC   RF,PDMPSASC         PSA SECONDS                                  
         AR    RE,RF                                                            
         LHI   RF,60                                                            
         SR    RF,RE               60-(COMM+PROMO+PSA SECONDS)                  
         LTR   RF,RF                                                            
         BP    *+6                                                              
         SR    RF,RF               DON'T ALLOW NEGATIVE                         
         STCM  RF,15,0(R2)                                                      
         MVC   4(4,R2),=AL4(60)    TOTAL MINUTE IN SECONDS                      
         B     INX                                                              
*                                                                               
*-----MINUTE OF PROGRAM                                                         
IMOP     DS    0H                                                               
         XC    0(8,R2),0(R2)                                                    
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'MOP',DBLOCKA,FULL                                 
         MVC   0(2,R2),FULL        MINUTE OF PROGRAM                            
         B     INX                                                              
*                                                                               
*-----EXACT TIME FOR MXM                                                        
ITIMEX   DS    0H                                                               
         XC    0(4,R2),0(R2)                                                    
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'TIMEEX',DBLOCKA,WORK                              
         MVC   0(4,R2),WORK+2                                                   
         OC    2(2,R2),2(R2)       IF END TIME IS ZERO                          
         BNZ   *+10                                                             
         MVC   2(2,R2),=AL2(2400)                                               
*                                                                               
         CLC   0(2,R2),=H'559'     ADJUST FOR 6AM START                         
         BH    ITIMEX02                                                         
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
ITIMEX02 DS    0C                                                               
         CLC   2(2,R2),=H'559'     ADJUST FOR 6AM END                           
         BH    ITIMEXX                                                          
         SR    RE,RE                                                            
         ICM   RE,3,2(R2)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,2(R2)                                                       
*                                                                               
ITIMEXX  B     INX                                                              
*                                                                               
*-----AVERAGE POD LENGTH                                                        
IAVGPODL DS    0H                                                               
         XC    0(8,R2),0(R2)                                                    
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   INX                                                              
         CLI   PDPDSAME,PDPDSAMQ    SAME POD AS PREVIOUS. SKIP                  
         BE    INX                                                              
         MVC   0(4,R2),PDPDSEC      POD LENGTH IN SECONDS                       
         OC    0(4,R2),0(R2)                                                    
         BZ    INX                                                              
         MVI   7(R2),1              COUNT AS 1 POD                              
         B     INX                                                              
*                                                                               
*-----GAPPED PROGRAM INDICATOR                                                  
IGAPPED  DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'GAPIND',DBLOCKA,FULL                              
         MVC   0(1,R2),FULL         GAPPED PROGRAM INDICATOR                    
         B     INX                                                              
*                                                                               
*-----CORRECTION DATE                                                           
ICORDAT  DS    0H                                                               
         CLC   =CL3'MXM ',PDSOURCE                                              
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'CORAMRLD',DBLOCKA,WORK                            
         OC    WORK(6),WORK                                                     
         BZ    INX                                                              
         GOTO1 DATCON,DMCB,(2,WORK+4),(0,(R2))                                  
         B     INX                                                              
*                                                                               
*-----CORRECTION TYPE                                                           
ICORTYP  DS    0H                                                               
         CLC   =CL3'MXM ',PDSOURCE                                              
         BNE   INX                                                              
         GOTO1 DEFINE,DMCB,=C'CORAMRLD',DBLOCKA,WORK                            
         MVC   0(4,R2),WORK                                                     
         B     INX                                                              
*                                                                               
*-----WEEK DATE                                                                 
IWKDAT   CLC   PDFILE(3),=CL3'NTI'                                              
         BE    IWKDAT2                                                          
         CLC   PDFILE(3),=CL3'RLD'                                              
         BE    IWKDAT2                                                          
         B     INX                                                              
IWKDAT2  GOTOR UNWEEK,DMCB,PDSBOOK,DUB                                          
         MVC   0(6,R2),DUB                                                      
         B     INX                                                              
*                                                                               
*-----ORDERED SUSTAINER INDICATOR                                               
IOS      DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'OSUSTAIN',DBLOCKA,FULL                            
         MVC   0(1,R2),FULL         ORDERED SUSTAINER INDICATOR                 
         B     INX                                                              
*                                                                               
*                                                                               
*                                                                               
* INPUT ROUTINE EXIT                                                            
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
* MISCELLANEOUS INPUT ROUTINES                                                  
* OUTPUT ROUTINES                                                               
*-----PROGRAM                                                                   
ONPG     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         TM    GLINDS,GLTOTLIN     SUPPRESS ON TOTAL TLINES                     
         BO    XIT                                                              
         MVC   LABLAREA(7),=C'PROGRAM'                                          
         CLI   GLARGS,C'C'                                                      
         BE    ONPG02                                                           
         CLI   GLARGS,C'S'                                                      
         BE    ONPG06                                                           
         CLI   GLARGS,C'B'                                                      
         BE    ONPG04                                                           
         MVC   CODEAREA(17),0(R2)                                               
         CLI   GLARGS,C'L'                                                      
         BNE   GENOUT                                                           
         MVC   CODEAREA(25),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
ONPG02   MVC   CODEAREA(6),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
ONPG04   MVC   CODEAREA(6),0(R2)                                                
         MVC   NAMEAREA(16),6(R2)                                               
         B     GENOUT                                                           
*                                                                               
ONPG06   MVC   CODEAREA(6),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----PROGRAM GROUP CODE                                                        
OPRGG    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'PRGGRP'                                           
         MVC   CODEAREA(8),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----EPISODE                                                                   
OEPI     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(7),=C'EPISODE'                                          
         MVC   NAMEAREA(16),0(R2)                                               
         CLI   GLARGS,C'L'                                                      
         BNE   GENOUT                                                           
         MVC   NAMEAREA(25),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----EPISODE (DDS ONLY) - FOR ITN EPISODE                                      
ODDSEPI  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(7),=C'EPISODE'                                          
         MVC   NAMEAREA(25),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----EPISODE                                                                   
OTRAK    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(8),=C'TRACKAGE'                                         
         MVC   NAMEAREA(16),0(R2)                                               
         CLI   GLARGS,C'L'                                                      
         BNE   GENOUT                                                           
         MVC   NAMEAREA(25),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----CABLE MERGED PPROGRAM NAME                                                
OCPROG   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(13),=C'CABLE PROGRAM'                                   
         MVC   NAMEAREA(16),0(R2)                                               
         CLI   GLARGS,C'L'                                                      
         BNE   GENOUT                                                           
         MVC   NAMEAREA(40),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----DAILY PROGRAM NAME                                                        
ODPRG    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(25),=CL25'DAILY PROGRAM'                                
         MVC   NAMEAREA(25),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----FILTER                                                                    
OFIL     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'FILTER'                                           
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----VIEWING TYPE                                                              
OVIEWT   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(12),=C'VIEWING TYPE'                                    
         MVC   CODEAREA(9),1(R2)                                                
OVIEWTX  B     GENOUT                                                           
*                                                                               
*-----BREAKOUT INDICATOR                                                        
OBKOUT   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----AIRING TYPE                                                               
OAIRT    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(1),0(R2)                                                
         CLI   MYLTYP,C'H'                                                      
         BNE   OAIRTX                                                           
OAIRT10  MVC   LABLAREA(1),0(R2)                                                
         CLI   0(R2),C'O'                                                       
         BNE   *+14                                                             
         MVC   CODEAREA(8),=C'ORIGINAL'                                         
         B     OAIRTX                                                           
         CLI   0(R2),C'R'                                                       
         BNE   *+14                                                             
         MVC   CODEAREA(6),=C'REPEAT'                                           
         B     OAIRTX                                                           
         CLI   0(R2),C'C'                                                       
         BNE   *+14                                                             
         MVC   CODEAREA(8),=C'COMBINED'                                         
         B     OAIRTX                                                           
         CLI   0(R2),C'M'                                                       
         BNE   *+14                                                             
         MVC   CODEAREA(8),=C'MULTIPLE'                                         
         B     OAIRTX                                                           
OAIRTX   B     GENOUT                                                           
*                                                                               
*-----CONTENT TYPE                                                              
OCONT    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(1),0(R2)                                                
         CLI   MYLTYP,C'H'                                                      
         BNE   OCONTX                                                           
OCONT10  MVC   LABLAREA(1),0(R2)                                                
         CLI   0(R2),C'R'                                                       
         BNE   *+14                                                             
         MVC   CODEAREA(7),=C'REGULAR'                                          
         B     OCONTX                                                           
         CLI   0(R2),C'S'                                                       
         BNE   *+14                                                             
         MVC   CODEAREA(7),=C'SPECIAL'                                          
         B     OCONTX                                                           
         CLI   0(R2),C'T'                                                       
         BNE   *+14                                                             
         MVC   CODEAREA(5),=C'TOTAL'                                            
         B     OCONTX                                                           
OCONTX   B     GENOUT                                                           
*                                                                               
*-----WEEK                                                                      
OWEK     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'WEEK'                                             
         MVC   CODEAREA(7),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----GAA                                                                       
OGAA     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'GAA?'                                             
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----ORIGNINATING STATION                                                      
OORIGIN  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'ORIGIN'                                           
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----GAA                                                                       
ODAILY   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'DAILY?'                                           
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----EPISODE NUMBER                                                            
OEPNUM   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(L'PDCSEN),0(R2)                                         
         B     GENOUT                                                           
*                                                                               
*-----Y/N FLAG                                                                  
OYESNO   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----NTILONG                                                                   
ONTILG   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(7),=C'NTILONG'                                          
         MVC   CODEAREA(10),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----PREMIERE                                                                  
OPREM    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'PREM'                                             
         MVC   CODEAREA(5),=C'N/A  '                                            
         OC    0(2,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         GOTO1 NETUNBK,DMCB,(C'W',(R2)),DUB,GETDAY,ADDAY,GETBROAD               
         GOTO1 DATCON,DMCB,(0,DUB),(7,CODEAREA)                                 
         MVC   CODEAREA+5(4),=C'     '                                          
         B     GENOUT                                                           
*                                                                               
*-----AVG?                                                                      
OAVGQ    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'AVG?'                                             
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----NTI CODE                                                                  
ONTI     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(3),=C'NTI'                                              
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----LNTI CODE          CALL + NTI                                             
OLNTI    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'LNTI'                                             
         MVC   CODEAREA(9),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----NTI CODE                                                                  
OTRNUM   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(3),=C'TRNUM'                                            
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
*          DATA SET NEPODDRIVE AT LEVEL 021 AS OF 08/28/00                      
*-----TCAST NUMBER                                                              
OTLNUM   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(3),=C'TLNUM'                                            
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----TP/NSI NETWORK AFFILIATES                                                 
OAFFIL   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(5),=C'AFFIL'                                            
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----TP/NSI USER AFFILIATES                                                    
OUFFIL   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'UAFFIL'                                           
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----COST                                                                      
OCOST    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (P8,(R2)),(10,(R3)),2,ZERO=BLANK                                 
         B     XIT                                                              
*                                                                               
*-----COVERAGE                                                                  
OCOVER   CLI   GLHOOK,GLINCOMP                                                  
         BNE   OCOV04                                                           
         CP    0(8,R2),=PL8'0'     CHECK FOR ZERO COVERAGE                      
         BE    OCOV02                                                           
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)     DEMO AMOUNT                                  
         DP    WORK(16),8(8,R2)      ROUND BY THE WEIGHT                        
         MVC   0(8,R2),WORK                                                     
*                                                                               
*--CHECK REMAINDER FOR ROUNDING                                                 
         MP    WORK+8(8),=PL1'2'                                                
         CP    WORK+8(8),8(8,R2)                                                
         BL    *+10                                                             
         AP    0(8,R2),=PL1'1'                                                  
*                                                                               
OCOV02   MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
OCOV04   EDIT  (P8,(R2)),(4,(R3)),ZERO=BLANK                                    
         B     XIT                                                              
*                                                                               
*-----STATION COUNT                                                             
OSCOUNT  CLI   GLHOOK,GLINCOMP                                                  
         BNE   OSCO04                                                           
         CP    0(8,R2),=PL8'0'     CHECK FOR ZERO COVERAGE                      
         BE    OSCO02                                                           
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)     DEMO AMOUNT                                  
         DP    WORK(16),8(8,R2)      ROUND BY THE WEIGHT                        
         MVC   0(8,R2),WORK                                                     
*                                                                               
*--CHECK REMAINDER FOR ROUNDING                                                 
         MP    WORK+8(8),=PL1'2'                                                
         CP    WORK+8(8),8(8,R2)                                                
         BL    *+10                                                             
         AP    0(8,R2),=PL1'1'                                                  
OSCO02   MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
OSCO04   EDIT  (P8,(R2)),(3,(R3)),ZERO=BLANK                                    
         B     XIT                                                              
*-----DAY                                                                       
ODAY     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(3),=C'DAY'                                              
*                                                                               
         CLC   =C'CTP',PDFILE                                                   
         BNE   *+14                                                             
         MVC   CODEAREA(3),=C'N/A'                                              
         B     GENOUT                                                           
*                                                                               
         CLI   GLARGS,C'L'                                                      
         BE    ODAY06                                                           
ODAY02   OC    2(3,R2),2(R2)                                                    
         BZ    ODAY04                                                           
         MVC   CODEAREA(3),2(R2)                                                
         MVC   0(3,R3),2(R2)                                                    
         B     GENOUT                                                           
*                                                                               
ODAY04   LA    R4,9                                                             
         L     R5,=A(DAYTAB)                                                    
         CLC   0(1,R2),0(R5)                                                    
         BE    *+14                                                             
         LA    R5,L'DAYTAB(R5)                                                  
         BCT   R4,*-14                                                          
         DC    H'0'                                                             
         MVC   2(3,R2),1(R5)                                                    
         B     ODAY02                                                           
*                                                                               
ODAY06   CLI   DAYPOPT,C'Y'                                                     
         BNE   ODAY08                                                           
         MVC   CODEAREA(3),2(R2)                                                
         MVC   0(3,R3),2(R2)                                                    
         B     GENOUT                                                           
*                                                                               
ODAY08   MVC   CODEAREA(1),1(R2)   MOVE SPOT DAY CODE                           
         GOTO1 DAYUNPK,DMCB,CODEAREA,(R3)                                       
         MVC   CODEAREA(8),0(R3)                                                
         B     GENOUT                                                           
*                                                                               
*-----ACTIVE DAYS                                                               
OADAY    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'DAYS'                                             
         MVC   CODEAREA(7),=C'  N/A  '                                          
*                                                                               
         CLC   =C'CNA',PDSOURCE                                                 
         BNE   OADAYX                                                           
         MVC   CODEAREA(7),=C'.......'                                          
         LA    RE,CODEAREA                                                      
*                                                                               
         CLI   PDSOURCE+4,C'T'     TIME PERIOD REQUEST                          
         BNE   OADAY3                                                           
         MVC   BYTE,0(R2)          DAY BITS ARE IN 1ST BYTE                     
         B     OADAY4                                                           
*                                  PROGRAM REQUEST                              
OADAY3   MVC   BYTE,1(R2)          COMPLETE DAY BITS ARE IN 2ND BYTE            
*                                                                               
OADAY4   LA    R1,X'40'            START WITH MONDAY                            
OADAY5   EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0                                                           
         BNO   OADAY20                                                          
*                                                                               
         LA    R4,9                GET CHAR DAY                                 
         L     R5,=A(DAYTAB)                                                    
         ZIC   R0,4(R5)            LOOK FOR SINGLE DAY BIT CODE                 
         CR    R1,R0                                                            
         BE    *+14                                                             
         LA    R5,L'DAYTAB(R5)                                                  
         BCT   R4,*-16                                                          
         DC    H'0'                                                             
         MVC   0(1,RE),1(R5)                                                    
*                                                                               
OADAY20  SRL   R1,1                                                             
         LTR   R1,R1                                                            
         LA    RE,1(RE)          ADVANCE OUTPUT CHAR IN CODEAREA                
         BNZ   OADAY5                                                           
*                                                                               
OADAYX   B     GENOUT                                                           
*                                                                               
*-----TIME                                                                      
OTIM     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'TIME'                                             
         XC    CODEAREA(11),CODEAREA                                            
*                                                                               
         CLC   0(4,R2),XFF                                                      
         BNE   *+14                                                             
         MVC   CODEAREA(11),=C'N/A        '                                     
         B     GENOUT                                                           
*                                                                               
         CLI   0(R2),X'FF'                                                      
         BNE   *+14                                                             
         MVC   CODEAREA(11),=CL11'ALL'                                          
         B     GENOUT                                                           
*                                                                               
         CLC   0(2,R2),=H'2400'                                                 
         BNH   OTIM02                                                           
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
OTIM02   CLC   2(2,R2),=H'2400'                                                 
         BNH   OTIM04                                                           
         SR    RE,RE                                                            
         ICM   RE,3,2(R2)                                                       
         SH    RE,=H'2400'                                                      
         STCM  RE,3,2(R2)                                                       
*                                                                               
OTIM04   DS    0C                                                               
         GOTO1 UNTIME,DMCB,(R2),CODEAREA                                        
         MVC   0(11,R3),CODEAREA                                                
         B     GENOUT                                                           
*                                                                               
*-----END TIME (COMSCORE)                                                       
OETIME   XC    CODEAREA(11),CODEAREA                                            
*                                                                               
         CLC   0(4,R2),XFF                                                      
         BNE   *+14                                                             
         MVC   CODEAREA(11),=C'N/A        '                                     
         B     GENOUT                                                           
*                                                                               
         CLI   0(R2),X'FF'                                                      
         BNE   *+14                                                             
         MVC   CODEAREA(11),=CL11'ALL'                                          
         B     GENOUT                                                           
*                                                                               
         CLC   0(2,R2),=H'2400'                                                 
         BNH   OETIM02                                                          
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
OETIM02  DS    0C                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R2)                                                    
         GOTO1 UNTIME,DMCB,FULL,CODEAREA                                        
         MVC   0(5,R3),CODEAREA                                                 
         B     GENOUT                                                           
*                                                                               
*-----START TIME (COMSCORE)                                                     
OSTIME   XC    CODEAREA(11),CODEAREA                                            
*                                                                               
         CLC   0(4,R2),XFF                                                      
         BNE   *+14                                                             
         MVC   CODEAREA(11),=C'N/A        '                                     
         B     GENOUT                                                           
*                                                                               
         CLI   0(R2),X'FF'                                                      
         BNE   *+14                                                             
         MVC   CODEAREA(11),=CL11'ALL'                                          
         B     GENOUT                                                           
*                                                                               
         CLC   0(2,R2),=H'2400'                                                 
         BNH   OSTIM02                                                          
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
OSTIM02  DS    0C                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R2)                                                    
         GOTO1 UNTIME,DMCB,FULL,CODEAREA                                        
         MVC   0(5,R3),CODEAREA                                                 
         B     GENOUT                                                           
*                                                                               
*-----MILITARY START TIME                                                       
OMILS    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'MILS'                                             
         MVC   CODEAREA(4),0(R2)                                                
*                                                                               
         CLC   =C'CTP',PDFILE                                                   
         BNE   *+10                                                             
         MVC   CODEAREA(4),=C'N/A '  DONT LOOK AT TIME FOR COUNTY COVG          
*                                                                               
         B     GENOUT                                                           
*                                                                               
*-----MILITARY END TIME                                                         
OMILE    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'MILE'                                             
         MVC   CODEAREA(4),0(R2)                                                
*                                                                               
         CLC   =C'CTP',PDFILE                                                   
         BNE   *+10                                                             
         MVC   CODEAREA(4),=C'N/A '  DONT LOOK AT TIME FOR COUNTY COVG          
*                                                                               
         B     GENOUT                                                           
*                                                                               
*-----ROUNDED START TIME                                                        
ORSTIM   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
*                                                                               
         CLC   =C'CTP',PDFILE      DONT LOOK AT TIME FOR COUNTY COVG            
         BNE   *+14                                                             
         MVC   CODEAREA(8),=CL8'N/A'                                            
         B     GENOUT                                                           
*                                                                               
         CLC   0(2,R2),=H'2400'                                                 
         BNL   ORST10                                                           
         CLC   0(2,R2),=H'1200'                                                 
         BNL   ORST20                                                           
ORST10   MVC   6(2,R3),=C'AM'                                                   
         B     *+10                                                             
ORST20   MVC   6(2,R3),=C'PM'                                                   
                                                                                
         CLC   0(2,R2),=H'1300'                                                 
         BL    ORST30                                                           
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SHI   RE,1200                                                          
         STCM  RE,3,0(R2)                                                       
         CLC   0(2,R2),=H'1300'                                                 
         BL    ORST30                                                           
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SHI   RE,1200                                                          
         STCM  RE,3,0(R2)                                                       
*                                                                               
ORST30   EDIT  (2,0(R2)),(4,WORK),FILL=0                                        
         MVC   0(2,R3),WORK        HH:MM                                        
         MVI   2(R3),C':'                                                       
         MVC   3(2,R3),WORK+2                                                   
         MVI   5(R3),C' '                                                       
*                                                                               
         MVC   CODEAREA(8),0(R3)                                                
*                                                                               
         B     GENOUT                                                           
*                                                                               
*-----COMSCORE NETWORK                                                          
OCNET    CLI   GLHOOK,GLINCOMP                                                  
         MVC   LABLAREA(7),=C'NETWORK'                                          
         MVC   CODEAREA(L'PDCSSTA),0(R2)                                        
         B     GENOUT                                                           
*                                                                               
*-----NETWORK NUMBER (COMSCORE)                                                 
ONETNUM  MVC   CODEAREA(L'PDCSNETN),0(R2)                                       
         B     GENOUT                                                           
*                                                                               
*-----NETWORK                                                                   
ONET     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(7),=C'NETWORK'                                          
         MVC   CODEAREA(7),0(R2)                                                
*                                                                               
         CLI   0(R2),C'S'          POSSIBLE INPUT RESEQUENCING                  
         BNE   GENOUT                                                           
         CLI   1(R2),32            NO - JUST EXIT                               
         BH    GENOUT                                                           
         L     R1,APODNET          YES - CONVERT THE STATION                    
         ZIC   RE,1(R2)             TO WHAT IT REALLY IS                        
         SLL   RE,3                X 8                                          
         AR    R1,RE                                                            
         MVC   CODEAREA(4),0(R1)                                                
         B     GENOUT                                                           
*                                                                               
*-----LONG NETWORK NAME                                                         
OLNET    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(7),=C'NETWORK'                                          
         MVC   NAMEAREA(30),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----FILE                                                                      
OFILE    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'FILE'                                             
         MVC   CODEAREA(3),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----SOURCE                                                                    
OSRCE    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'SOURCE'                                           
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----MARKET                                                                    
OMRKT    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'MARKET'                                           
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----MARKET NAME                                                               
OMRKTNAM CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(8),=C'MKT NAME'                                         
         MVC   NAMEAREA(24),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----ALPHA MARKET CODE                                                         
OMRKTALF CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(5),=C'MRKT '                                            
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----PROGRAM TYPE                                                              
OPTYP    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'TYPE'                                             
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----PROGRAM TYPE (4 CHAR)                                                     
OPTYPL   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'TYP4'                                             
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----PROGRAM TYPE (SUB)                                                        
OPTYPS   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'TYPS'                                             
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----PROGRAM RECORD NEW/RETURNING INDICATOR                                    
OPGNEW   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'NEW?'                                             
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----PROGRAM RECORD TIER                                                       
OPGTIER  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'TIER'                                             
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----PROGRAM RECORD CONTENT RATING CODE                                        
OPGRAT   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'RATE'                                             
         MVC   CODEAREA(3),=C'TV-'                                              
         MVC   CODEAREA+3(2),0(R2)                                              
         B     GENOUT                                                           
*                                                                               
*-----PROGRAMMING SOURCE                                                        
OPSOUR   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'PSRC'                                             
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----FEED                                                                      
OFEED    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'FEED'                                             
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----LIVE                                                                      
OLIVE    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(1),0(R2)                                                
         CLI   MYLTYP,C'H'                                                      
         BE    OLIV10                                                           
         CLI   MYLTYP,C'M'                                                      
         BE    OLIV10                                                           
         B     OLIVEX                                                           
OLIV10   CLI   0(R2),C'L'                                                       
         BNE   *+10                                                             
         MVC   CODEAREA(4),=C'LIVE'                                             
OLIVEX   B     GENOUT                                                           
*                                                                               
*----COMMERCIAL STATUS                                                          
OCOMM    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'COMM'                                             
         MVC   CODEAREA(3),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----STATION                                                                   
OSTAT    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(8),=C'STATION '                                         
         MVC   CODEAREA(8),0(R2)                                                
*                                                                               
         CLI   0(R2),C'S'          POSSIBLE INPUT RESEQUENCING                  
         BNE   GENOUT                                                           
         CLI   1(R2),32            NO - JUST EXIT                               
         BH    GENOUT                                                           
         L     R1,APODNET          YES - CONVERT THE STATION                    
         ZIC   RE,1(R2)             TO WHAT IT REALLY IS                        
         SLL   RE,3                X 8                                          
         AR    R1,RE                                                            
         MVC   CODEAREA(4),0(R1)                                                
         B     GENOUT                                                           
*                                                                               
*-----DAYPART CODE (7 CHARS)                                                    
ODPT     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(7),=C'DAYPART'                                          
         MVC   CODEAREA(7),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----PURE                                                                      
OPURE    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'PURE'                                             
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----PURE3                                                                     
OPURE3   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(5),=C'PURE3'                                            
         MVC   CODEAREA(3),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----SIU                                                                       
OSIU     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(3),=C'SIU'                                              
         MVC   CODEAREA(11),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----QUARTER HOUR DURATION                                                     
OQHDUR   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(3),=C'QTR'                                              
         MVC   CODEAREA(3),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----SID PERIOD                                                                
OPERIOD  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'PERIOD'                                           
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----SID PROGRAM TYPE EXPANSION                                                
OPRGTYP  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(7),=C'PRG TYP'                                          
         MVC   CODEAREA(7),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----MARKET GROUP ID                                                           
OGRPID   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(5),=C'GRPID'                                            
*                                                                               
         L     RF,=A(SPMGRTAB)                                                  
         LHI   RE,(SPMGRTBX-SPMGRTAB)/L'SPMGRTAB                                
         CLC   0(1,R2),2(RF)                                                    
         BE    OGRPID2                                                          
         AHI   RF,L'SPMGRTAB                                                    
         BCT   RE,*-14                                                          
         MVI   CODEAREA,C'?'                                                    
*                                                                               
OGRPID2  MVC   CODEAREA(2),0(RF)                                                
         CLI   CODEAREA+1,C' '                                                  
         BNE   *+14                                                             
         MVC   CODEAREA+1(4),1(R2)                                              
         B     *+10                                                             
         MVC   CODEAREA+2(4),1(R2)                                              
         B     GENOUT                                                           
*                                                                               
*-----EFFECTIVE DATE                                                            
OEFFDAT  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'EFFECT'                                           
         MVC   CODEAREA(5),0(R2)                                                
         CLC   PDSOURCE,=C'IUN'                                                 
         BNE   GENOUT                                                           
         MVC   CODEAREA(6),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----WEIGHT                                                                    
OWEIGHT  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(6),=C'WEIGHT'                                           
         MVC   CODEAREA(6),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----BOOK                                                                      
OBOOK    CLI   GLARGS,C'R'                                                      
         BNE   *+10                                                             
         XC    0(8,R2),XFF                                                      
         CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'BOOK'                                             
         MVC   CODEAREA(6),2(R2)                                                
         MVC   0(6,R3),2(R2)                                                    
         B     GENOUT                                                           
*                                                                               
*-----DEMO                                                                      
ODEM     BRAS  RE,ODMS                                                          
         B     XIT                                                              
*                                                                               
*-----TIMES AIRED                                                               
ORUN     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     XIT                                                              
*                                                                               
*-----TOTAL WEIGHT                                                              
OWT      CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (B4,0(R2)),(8,0(R3)),ZERO=BLANK   OUTPUT TOT WEIGHT              
         MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
*-----TOTAL DURATION                                                            
ODURTOT  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (B4,0(R2)),(8,0(R3)),ZERO=BLANK   OUTPUT THE RUN                 
*                                                                               
         MVI   GLHOOK,GLIDID       LET DRIVER EDIT                              
         B     XIT                                                              
*                                                                               
*-----MONITOR PLUS COST                                                         
OMPDOLO  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (B4,0(R2)),(8,0(R3)),ZERO=BLANK   OUTPUT THE RUN                 
*                                                                               
         MVI   GLHOOK,GLIDID       LET DRIVER EDIT                              
         B     XIT                                                              
*                                                                               
*-----MONITOR PLUS COST                                                         
OMPDOL   CLI   GLHOOK,GLINCOMP                                                  
         BE    OMP10                                                            
         EDIT  (P8,(R2)),(8,(R3)),ZERO=BLANK                                    
         B     XIT                                                              
*                                                                               
OMP10    CLI   GLHOOK,GLINCOMP                                                  
         BNE   OMP10                                                            
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)                                                  
         CP    8(8,R2),=PL8'0'                                                  
         BE    XIT                                                              
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)                                                  
         DP    WORK(16),8(8,R2)                                                 
         MVC   0(8,R2),WORK                                                     
*--CHECK REMAINDER FOR ROUNDING                                                 
         MP    WORK+8(8),=PL1'2'                                                
         CP    WORK+8(8),8(8,R2)                                                
         BL    *+10                                                             
         AP    0(8,R2),=PL1'1'                                                  
         MVI   GLHOOK,GLIDID       LET DRIVER EDIT                              
         B     XIT                                                              
*                                                                               
*-----RUN DATE                                                                  
ORDT     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         CLC   PDFILE(3),=CL3'NTI'                                              
         BE    *+10                                                             
         CLC   PDFILE(3),=CL3'RLD'                                              
         BE    *+10                                                             
         CLC   =CL3'NAW',PDSOURCE                                               
         BE    *+10                                                             
         CLC   =CL3'BBM',PDSOURCE                                               
         BE    *+10                                                             
         CLC   =CL3'CSI',PDSOURCE                                               
         BNE   ORDT02                                                           
         OC    0(6,R2),0(R2)       JUST IN CASE IT WASN'T FILLED IN             
         BZ    ORDT02                                                           
         GOTO1 DATCON,DMCB,(0,(R2)),(0,DUB)   YYMMDD                            
         MVC   CODEAREA(6),DUB                                                  
         CLI   CODEAREA,C'9'                                                    
         BNH   *+16                                                             
         IC    R0,CODEAREA         CONVERT DECADE CODE TO PRINTABLE             
         SH    R0,=H'10'                                                        
         STC   R0,CODEAREA                                                      
*                                                                               
         B     GENOUT                                                           
*                                                                               
ORDT02   MVI   GLHOOK,GLIDID       N/A FOR OTHER FILES                          
         B     XIT                                                              
*                                                                               
*----- DATE (INPUT IS IRDT, AS FOR RUN DATE)                                    
ODATE    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         CLC   PDFILE(3),=CL3'NTI'                                              
         BE    *+10                                                             
         CLC   PDFILE(3),=CL3'RLD'                                              
         BE    *+10                                                             
         CLC   =CL3'NAW',PDSOURCE                                               
         BE    *+10                                                             
         CLC   =CL3'BBM',PDSOURCE                                               
         BE    *+10                                                             
         CLC   =CL3'COM',PDSOURCE                                               
         BE    *+10                                                             
         CLC   =CL3'CSI',PDSOURCE                                               
         BNE   ODATE2                                                           
         MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     XIT                                                              
*                                                                               
ODATE2   MVI   GLHOOK,GLIDID       N/A FOR OTHER FILES                          
         B     XIT                                                              
*                                                                               
*-----STACKED DEMOS                                                             
OSTDEM   MVI   BYTE,0              SET UP DEMO BUFFER                           
*                                                                               
         CLI   PDMETHOD,6          METHOD=AE                                    
         BNE   OST01A                                                           
         MVC   SVTOTWT,16(R2)      SAVE TOTAL WEIGHT                            
         B     OST01B                                                           
*                                                                               
OST01A   MVC   FULL(2),22(R2)      GET SOURCE INDICATOR                         
         MVC   FULL+2(2),30(R2)    GET MEDIA INDICATOR                          
OST01B   LA    R4,PDSTADEF                                                      
         L     RE,PDADEMTB         SET UP DEMO BUFFER                           
         USING PDDMBUFF,RE                                                      
         LA    R5,PODDEMO+3        GET FIRST DEMO OF STACK                      
         ZIC   RF,GLARGS          DEMO OFFSET                                   
         ZIC   R1,PODDMENT        NUMBER OF DEMOS IN A SERIES                   
         CLI   PDOVSYS,3                                                        
         BNE   *+8                                                              
         LA    R5,1(R5)            ONE FOR FIRST DEMO                           
         MH    R1,LNDEMCOD                                                      
*                                                                               
         CLI   GLARGS,0                                                         
         BE    *+10                                                             
         AR    R5,R1                                                            
         BCT   RF,*-2                                                           
         DROP  RE                                                               
*                                                                               
         CLI   GLHOOK,GLINCOMP                                                  
         BNE   OSTD10                                                           
*                                                                               
OSTD01   CLI   PDDUROPT,C'M'                                                    
         BNE   OSTD02                                                           
         CLI   1(R4),0                                                          
         BE    OSTD08                                                           
*                                                                               
OSTDAE   CLI   PDMETHOD,6          METHOD=AE,UNWEIGHT BEFORE DIVISION           
         BNE   OSTDAEX                                                          
         XC    WORK(8),WORK                                                     
         MVC   WORK+8(8),0(R2)                                                  
         OC    SVTOTWT,SVTOTWT                                                  
         BZ    OSTDAE05                                                         
         CP    SVTOTWT,=PL8'0'                                                  
         BNE   *+10                                                             
OSTDAE05 MVC   SVTOTWT,=PL8'1'                                                  
         DP    WORK(16),SVTOTWT      DIVIDE FIRST DEMO BY WEIGHT                
         MVC   0(8,R2),WORK                                                     
         XC    WORK(8),WORK                                                     
         MP    WORK(16),=PL1'2'      AND ROUND                                  
         CP    WORK(16),SVTOTWT                                                 
         BL    *+10                                                             
         AP    0(8,R2),=PL1'1'                                                  
*                                                                               
         XC    WORK(8),WORK                                                     
         MVC   WORK+8(8),8(R2)                                                  
         DP    WORK(16),SVTOTWT      DIVIDE SECOND DEMO BY WEIGHT               
         MVC   8(8,R2),WORK                                                     
         XC    WORK(8),WORK                                                     
         MP    WORK(16),=PL1'2'      AND ROUND                                  
         CP    WORK(16),SVTOTWT                                                 
         BL    *+10                                                             
         AP    8(8,R2),=PL1'1'                                                  
OSTDAEX  DS    0X                                                               
*                                                                               
         CLI   1(R4),X'C1'                                                      
         BNL   OSTD1A                                                           
         CLI   1(R4),130           SPACE                                        
         BNE   OSTD04                                                           
         ZAP   8(8,R2),=PL8'0'                                                  
         B     OSTD06                                                           
*                                                                               
OSTD1A   CP    8(8,R2),=PL8'0'     CHECK FOR ZERO DEMO                          
***      BE    XIT                 BUG - CK OTHER DEMOS FOLLOWING EVEN          
         BE    OSTD06               IF THIS DEMO IS ZERO                        
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)     DEMO AMOUNT                                  
         CLI   1(R4),C'N'          GAA IMPRESSIONS                              
         BE    *+8                                                              
         CLI   1(R4),C'H'          CABLE IMPRESSIONS                            
         BE    *+8                                                              
         CLI   1(R4),C'T'          IMPRESSIONS                                  
         BNE   OSTD01B                                                          
         MVC   WORK+8(8),0(R2)     DEMO AMOUNT                                  
         CLI   PRECOPT,C'Y'        CABLE PRECISION                              
         BE    *+8                                                              
         CLI   1(R4),C'H'          CABLE IMPRESSIONS                            
         BNE   *+10                                                             
         MP    WORK(16),=PL2'10'   INCREASE BY 10 TIMES                         
         MP    WORK(16),=PL2'10'                                                
         DP    WORK(16),8(8,R2)      ROUND BY THE WEIGHT                        
         MVC   WORK+8(8),WORK                                                   
         XC    WORK(8),WORK                                                     
         AP    WORK(16),=PL3'5000'                                              
         DP    WORK(16),=PL8'10000'                                             
         MVC   0(8,R2),WORK                                                     
         B     OSTD06                                                           
OSTD01B  CLI   1(R4),C'H'          CABLE IMPRESSIONS                            
         BNE   OSTD01C                                                          
         AP    WORK(16),=PL2'50'                                                
         DP    WORK(16),=PL8'100'                                               
         MVC   0(8,R2),WORK                                                     
         B     OSTD06                                                           
OSTD01C  CP    8(8,R2),=PL8'0'     CHECK FOR ZERO DEMO                          
***      BE    XIT                 BUG - CK OTHER DEMOS FOLLOWING EVEN          
         BE    OSTD06               IF THIS DEMO IS ZERO                        
         CLI   PDGSTACK,X'FF'      STACKING GENDER DEMOS?                       
         BE    OCTD01E              YES. CHECKS BELOW DON'T APPLY               
         CLI   1(R4),C'V'          ADJUST FOR VPH                               
         BE    *+12                                                             
         CLI   1(R4),C'M'          AND GAA VPH                                  
         BNE   *+10                                                             
         MP    WORK(16),=PL3'1000'                                              
OCTD01E  CLI   PRECOPT,C'Y'        CABLE PRECISION                              
         BNE   OSTD04A                                                          
         BRAS  RE,STSYZ            CK IF COMPUTING SHARE AS S=Y/Z               
         BE    OSTD01F                                                          
         CLI   1(R4),C'L'                                                       
         BE    *+12                                                             
         CLI   1(R4),C'R'                                                       
         BNE   OSTD04A                                                          
OSTD01F  MP    WORK(16),=PL3'10'   INCREASE PREC TO 2DEC                        
         B     OSTD04A                                                          
*                                                                               
OSTD02   CLI   1(R4),0                                                          
         BE    OSTD08                                                           
         CLI   1(R4),X'C1'                                                      
         BNL   OSTD04                                                           
         CLI   1(R4),130           SPACE                                        
         BNE   OSTD04                                                           
         ZAP   8(8,R2),=PL8'0'                                                  
         B     OSTD06                                                           
*                                                                               
OSTD04   CP    8(8,R2),=PL8'0'     CHECK FOR ZERO DEMO                          
         BE    OSTD06                                                           
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)     DEMO AMOUNT                                  
                                                                                
OSTD04A  DS    0H                                                               
OSTD04AA DP    WORK(16),8(8,R2)    ROUND BY THE WEIGHT                          
         MVC   0(8,R2),WORK                                                     
*--CHECK REMAINDER FOR ROUNDING                                                 
         XC    WORK(8),WORK                                                     
         MP    WORK(16),=PL1'2'                                                 
         CP    WORK(16),8(8,R2)                                                 
         BL    *+10                                                             
         AP    0(8,R2),=PL1'1'                                                  
*                                                                               
OSTD06   LA    R2,16(R2)                                                        
         LA    R4,2(R4)                                                         
         CLI   BYTE,0              CHECK FOR FIRST PASS                         
         BNE   OSTD01                                                           
         LA    R2,16(R2)           BUMP PAST SOURCE INDICATOR                   
         MVI   BYTE,X'FF'                                                       
         B     OSTD01                                                           
*                                                                               
OSTD08   MVI   GLHOOK,GLIDID       TELL DRIVER YOU JUST DID THIS                
         B     XIT                                                              
*                                                                               
OSTD10   CLI   1(R4),0                                                          
         BE    XIT                                                              
         CLI   1(R4),X'C1'                                                      
         BNL   OSTD12                                                           
         CLI   1(R4),130           SPACE                                        
         BE    OSTD18                                                           
*                                                                               
OSTD12   CLI   GLARGS,0                                                         
         BNE   OSTD14                                                           
         CLI   BYTE,X'FF'          ONLY CHECK ON FIRST PASS                     
         BE    OSTD14                                                           
         CP    0(8,R2),MAXOPT      CHECK DEMO ABOVE THE MAXIMUM                 
         BH    OSTD20                                                           
         CP    0(8,R2),MINOPT      CHECK DEMO BELOW THE MINIMUM                 
         BL    OSTD20                                                           
         CLI   SOLOOPT,C'N'                                                     
         BNE   *+14                                                             
         CP    8(8,R2),=PL1'1'     DONT PRINT SINGLE RUN SHOWS                  
         BE    OSTD20                                                           
*                                                                               
OSTD14   LR    RF,R5                                                            
         BRAS  RE,EDITIT                                                        
         BE    OSTD18                                                           
         CLI   PRECOPT,C'Y'                                                     
         BE    OSTD16                                                           
         EDIT  (P8,(R2)),(7,(R3)),ZERO=BLANK                                    
         B     OSTD18                                                           
*                                                                               
OSTD16   EDIT  (P8,(R2)),(7,(R3)),1,ZERO=BLANK                                  
*                                                                               
OSTD18   LA    R2,16(R2)           BUMP TO NEXT DEMO                            
         LA    R4,2(R4)            BUMP TO NEXT STACK ENTRY                     
         AH    R5,LNDEMCOD         BUMP TO NEXT DEMO DEFINITION                 
         LA    R3,198(R3)          BUMP TO NEXT OUTPUT LINE                     
         CLI   BYTE,0              CHECK FOR FIRST PASS                         
         BNE   OSTD10                                                           
         LA    R2,16(R2)          BUMP PAST SOURCE/MEDIA INDICATOR              
         MVI   BYTE,X'FF'                                                       
         B     OSTD10                                                           
*                                                                               
OSTD20   MVI   PRTSW,C'N'                                                       
         B     XIT                                                              
*-----DEMO/TIMES AIRED                                                          
ODEMAIR  CLI   GLHOOK,GLINCOMP                                                  
         BNE   ODEM02                                                           
         BRAS  RE,ODMS                                                          
         MVI   GLHOOK,GLIDID       TELL DRIVER YOU JUST DID THIS                
         B     XIT                                                              
*                                                                               
ODEM02   BRAS  RE,ODMS             OUTPUT THE DEMO                              
         EDIT  (P8,32(R2)),(5,8(R3)),ZERO=BLANK   OUTPUT THE RUN                
         B     XIT                                                              
*                                                                               
*-----DEMO/REPEAT FLAG                                                          
ODEMRPT  CLI   GLHOOK,GLINCOMP                                                  
         BNE   ODEMRPT2                                                         
         CVB   RE,24(0,R2)                                                      
         ST    RE,FULL                                                          
         BRAS  RE,ODMS                                                          
         MVI   GLHOOK,GLIDID       TELL DRIVER YOU JUST DID THIS                
         B     XIT                                                              
*                                                                               
ODEMRPT2 BRAS  RE,ODMS             OUTPUT THE DEMO                              
         CVB   R1,32(R2)                                                        
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         MVI   8(R3),C'*'          OUTPUT THE FLAG                              
         B     XIT                                                              
*                                                                               
*-----STACK DATA                                                                
OSTDATA  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         LA    R5,PDSTADEF                                                      
         MVC   FULL,=A(NOSTDLST)                                                
         CLI   PDGSTACK,X'FF'      STACKING GENDER DEMOS?                       
         BNE   ODSTA02             NO                                           
         MVC   FULL,=A(GENTABLE)   YES, USE GENTABLE                            
*                                                                               
ODSTA02  ICM   R4,15,FULL                                                       
         CLI   1(R5),0                                                          
         BE    XIT                                                              
*                                                                               
ODSTA04  CLC   1(1,R5),0(R4)                                                    
         BNE   ODSTA05                                                          
         MVC   0(7,R3),1(R4)                                                    
         LA    R3,198(R3)                                                       
         LA    R5,2(R5)                                                         
         B     ODSTA02                                                          
*                                                                               
ODSTA05  LA    R4,8(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   ODSTA04                                                          
         DC    H'0'                                                             
*                                                                               
*-----SQAD QUARTER                                                              
OSQART   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(5),=C'SQART'                                            
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----COUNTY NAME                                                               
OCTYNAM  DS    0H                                                               
         CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(8),=C'CTY NAME'                                         
         MVC   CODEAREA(20),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----COUNTY NUMBER                                                             
OCTYNO   DS    0H                                                               
         CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (2,(R2)),(3,(R3)),ZERO=NOBLANK,FILL=0                            
         B     XIT                                                              
*                                                                               
*-----DMA NUMBER                                                                
ODMANO   DS    0H                                                               
         CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (2,(R2)),(3,(R3)),ZERO=NOBLANK,FILL=0                            
         B     XIT                                                              
*                                                                               
*-----DMA OF ORIGIN                                                             
ODMAORG  DS    0H                                                               
         CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (2,(R2)),(3,(R3)),ZERO=NOBLANK,FILL=0                            
         B     XIT                                                              
*                                                                               
*-----DMA NAME                                                                  
ODMANAM  DS    0H                                                               
         CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(8),=C'DMA NAME'                                         
         MVC   CODEAREA(26),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----STATE                                                                     
OSTATE   DS    0H                                                               
         CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(8),=C'STATE   '                                         
         MVC   CODEAREA(2),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----STATE NAME                                                                
OSTANAM  DS    0H                                                               
         CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(8),=C'STATE   '                                         
         MVC   CODEAREA(25),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*-----SYSCODE                                                                   
OSYSC    DS    0H                                                               
         CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (2,0(R2)),(4,0(R3)),ZERO=BLANK                                   
         B     XIT                                                              
*                                                                               
*----UNIVERSE YEAR                                                              
OUYEAR   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (2,0(R2)),(4,0(R3)),ZERO=BLANK                                   
         B     XIT                                                              
*                                                                               
*----AUDIENCE ESTIMATOR FLAG                                                    
OAE      CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         MVC   0(2,R3),=C'AE'                                                   
         CLI   0(R2),C'A'                                                       
         BL    XIT                 NOT A CHARACTER. EXIT.                       
         MVC   2(1,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
*----LOAD DATE                                                                  
OLDDAT   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         GOTO1 DATCON,DMCB,(2,(R2)),(8,(R3))                                    
         B     XIT                                                              
*                                                                               
*-----INPUT VIEWING TYPE                                                        
OIVTYP   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         TM    GLINDS,GLTOTLIN     SUPPRESS ON TOTAL TLINES                     
         BO    XIT                                                              
         MVC   0(1,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
*-----COMMERCIAL DURATION                                                       
OCDUR    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (B4,0(R2)),(9,0(R3)),ZERO=NOBLANK                                
         MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
*-----NUMBER OF COMMERCIAL TELECASTS                                            
OCTCAST  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (B1,0(R2)),(5,0(R3)),ZERO=NOBLANK                                
         MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
*----PROGRAM RECORDS VIEWING SOURCE AND TYPE                                    
OPVS     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         MVC   0(L'PDPVS,R3),0(R2)                                              
         B     XIT                                                              
*                                                                               
*-----MARKET BREAK INFO                                                         
ONADCDUR CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (B4,0(R2)),(9,0(R3)),ZERO=BLANK                                  
         MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
*-----PROGRAM MINUTE                                                            
OPRMIN   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
*                                                                               
         OC    0(2,R2),0(R2)                                                    
         BNZ   *+14                                                             
         MVC   CODEAREA(5),=CL5'ALL'                                            
         B     OPRMN20                                                          
*                                                                               
         CLC   0(2,R2),=H'2400'     CONVERT 2401-2959 TO                        
         BNH   OPRMN10                      0001-0559                           
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
OPRMN10  XC    FULL,FULL            END TIME=0                                  
         MVC   FULL(2),0(R2)        START TIME                                  
         GOTO1 UNTIME,DMCB,FULL,WORK                                            
         MVC   CODEAREA(5),WORK                                                 
*                                                                               
OPRMN20  MVC   LABLAREA(14),=C'PROGRAM MINUTE'                                  
         B     GENOUT                                                           
*                                                                               
*-----COMMERCIAL FLAG                                                           
OCOMMF   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(4),=CL4' '                                              
         MVC   CODEAREA(2),0(R2)                                                
         MVC   LABLAREA(4),=C'COM?'                                             
         B     GENOUT                                                           
*                                                                               
*-----PROMO FLAG                                                                
OPROMOF  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(4),=CL4' '                                              
         MVC   CODEAREA(1),0(R2)                                                
         MVC   LABLAREA(4),=C'PRO?'                                             
         B     GENOUT                                                           
*                                                                               
*-----PSA FLAG                                                                  
OPSAF    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(4),=CL4' '                                              
         MVC   CODEAREA(1),0(R2)                                                
         MVC   LABLAREA(4),=C'PSA?'                                             
         B     GENOUT                                                           
*                                                                               
*-----PROGRAM+ FLAG                                                             
OPGPF    CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(4),=CL4' '                                              
         MVC   CODEAREA(1),0(R2)                                                
         MVC   LABLAREA(4),=C'PG+?'                                             
         B     GENOUT                                                           
*                                                                               
*-----POD START/END TIME                                                        
OPDTIME  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
*                                                                               
         OC    0(2,R2),0(R2)                                                    
         BZ    XIT                                                              
*                                                                               
         CLC   0(2,R2),=H'2400'     CONVERT 2401-2959 TO                        
         BNH   OPDTM10                      0001-0559                           
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
OPDTM10  XC    FULL,FULL            END TIME=0                                  
         MVC   FULL(2),0(R2)        START TIME                                  
         GOTO1 UNTIME,DMCB,FULL,WORK                                            
         MVC   CODEAREA(5),WORK                                                 
         MVC   LABLAREA(8),=C'POD TIME'                                         
         B     GENOUT                                                           
*                                                                               
*-----POD LENGTH                                                                
OPDLEN   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (B2,0(R2)),(3,0(R3)),ZERO=BLANK                                  
         MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
*-----POD NUMBER                                                                
OPDNUM   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (B2,0(R2)),(3,0(R3)),ZERO=BLANK                                  
         MVC   CODEAREA(3),0(R3)                                                
         MVC   LABLAREA(5),=C'POD #'                                            
         B     GENOUT                                                           
*                                                                               
*-----% COMMERCIAL SECONDS                                                      
OPCOMM   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         SR    R0,R0                                                            
         ICM   R1,15,0(R2)         TOTAL COMMERCIAL SECONDS                     
         MHI   R1,100              *100                                         
         ICM   RE,15,4(R2)         TOTAL SECONDS                                
         BZ    XIT                                                              
         DR    R0,RE               TOT COMM SECS * 100/TOTAL SECONDS            
         MHI   R0,2                PERFORM ROUNDING                             
         CR    R0,RE                                                            
         BL    *+8                                                              
         AHI   R1,1                                                             
         EDIT  (R1),(5,0(R3)),ZERO=BLANK,TRAIL=C'%'                             
         MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
*-----% PROMO SECONDS                                                           
OPPROMO  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         SR    R0,R0                                                            
         ICM   R1,15,0(R2)         PROMO SECONDS                                
         MHI   R1,100              *100                                         
         ICM   RE,15,4(R2)         TOTAL SECONDS                                
         BZ    XIT                                                              
         DR    R0,RE               PROMO SECS * 100/TOTAL SECONDS               
         MHI   R0,2                PERFORM ROUNDING                             
         CR    R0,RE                                                            
         BL    *+8                                                              
         AHI   R1,1                                                             
         EDIT  (R1),(6,0(R3)),ZERO=BLANK,TRAIL=C'%'                             
         MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
*-----AVERAGE POD LENGTH                                                        
OAVGPODL CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         SR    RE,RE                                                            
         ICM   RF,15,0(R2)         POD SECONDS                                  
         ICM   R0,15,4(R2)         NUMBER OF PODS                               
         BZ    XIT                                                              
         DR    RE,R0               POD SECONDS/NUMBER OF PODS                   
         MHI   RE,2                PERFORM ROUNDING                             
         CR    RE,R0                                                            
         BL    *+8                                                              
         AHI   RF,1                AVERAGE POD LENGTH IN SECONDS                
*                                                                               
         SR    RE,RE               CONVERT TO MM:SS                             
         D     RE,=A(60)           RF=MINUTES, RE=SECONDS                       
*                                                                               
         EDIT  (RF),(2,0(R3)),ZERO=NOBLANK,FILL=0                               
         MVI   2(R3),C':'                                                       
         EDIT  (RE),(2,3(R3)),ZERO=NOBLANK,FILL=0                               
         MVI   GLHOOK,GLIDID                                                    
         B     XIT                                                              
*                                                                               
*-----MINUTE OF PROGRAM                                                         
OMOP     CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         EDIT  (B2,0(R2)),(4,0(R3)),ZERO=BLANK                                  
         MVC   CODEAREA(4),0(R3)                                                
         MVC   LABLAREA(5),=C'MOP #'                                            
         B     GENOUT                                                           
*                                                                               
*-----EXACT TIME FOR MXM                                                        
OTIMEX   CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(4),=C'TIME'                                             
         XC    CODEAREA(11),CODEAREA                                            
*                                                                               
         CLC   0(2,R2),=H'2400'                                                 
         BNH   OTIMEX02                                                         
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SH    RE,=H'2400'                                                      
         STCM  RE,3,0(R2)                                                       
*                                                                               
OTIMEX02 CLC   2(2,R2),=H'2400'                                                 
         BNH   OTIMEX04                                                         
         SR    RE,RE                                                            
         ICM   RE,3,2(R2)                                                       
         SH    RE,=H'2400'                                                      
         STCM  RE,3,2(R2)                                                       
*                                                                               
OTIMEX04 DS    0C                                                               
         GOTO1 UNTIME,DMCB,(R2),CODEAREA                                        
         MVC   0(11,R3),CODEAREA                                                
         B     GENOUT                                                           
*                                                                               
*-----GAPPED PROGRAM INDICATOR                                                  
OGAPPED  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(7),=CL7' '                                              
         MVC   CODEAREA(1),0(R2)                                                
         MVC   LABLAREA(7),=C'GAPPED?'                                          
         B     GENOUT                                                           
*                                                                               
*-----CORRECTION DATE                                                           
OCORDAT  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(13),=C'CORRECTN DATE'                                   
         OC    0(6,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         GOTO1 DATCON,DMCB,(0,(R2)),(8,CODEAREA)                                
         B     GENOUT                                                           
*                                                                               
*-----CORRECTION TYPE                                                           
OCORTYP  CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   LABLAREA(13),=C'CORRECTN TYPE'                                   
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*-----ORDERED SUSTAINER INDICATOR                                               
OOS      CLI   GLHOOK,GLINCOMP                                                  
         BE    XIT                                                              
         MVC   CODEAREA(7),=CL7' '                                              
         MVC   CODEAREA(1),0(R2)                                                
         MVC   LABLAREA(7),=C'OS?'                                              
         B     GENOUT                                                           
*                                                                               
*                                                                               
         EJECT                                                                  
*-----HEADLINE ROUTINES                                                         
*-----DEMO                                                                      
HDEM     BAS   RE,HDMS                                                          
         B     XIT                                                              
HDMS     NTR1                                                                   
         BRAS  RE,PUTHDEM          PUT DEMO HEADINGS                            
         B     HFILTER             CHECK FOR FILTER LINE                        
*                                                                               
*-----DEMO/TIMES AIRED                                                          
HDEMAIR  BAS   RE,HDMS             MOVE DEMO HEADLIINE                          
         MVC   8(5,R3),=C'AIRED'                                                
         MVI   GLAUTOCH,C'N'                                                    
         B     XIT                                                              
*                                                                               
*-----DEMO/REPEAT                                                               
HDEMRPT  BAS   RE,HDMS             MOVE DEMO HEADLIINE                          
         MVC   8(2,R3),=C'RF'                                                   
         MVI   GLAUTOCH,C'N'                                                    
         B     XIT                                                              
*                                                                               
*-----STACK DEMO                                                                
HSTDEM   MVC   DUB(5),=CL5'STACK'  SET UP STACK INDICATOR                       
         CLI   PDGSTACK,X'FF'      USING GENDER STACK?                          
         BNE   *+10                NO                                           
         MVC   DUB(6),=CL6'GSTACK' YES                                          
         B     HDEM                                                             
*                                                                               
*-----FILTER LINE                                                               
HFILTER  BAS   RE,CHKROOM          SEE IF ROOM IN HEADLINE FOR FILTER           
         LTR   R2,R2               CONT NUMBERS OF LINES LEFT IN HEADS          
         BZ    XIT                                                              
*                                                                               
         CLI   GLARGS+15,0         ONLY HANDLING DATES FOR NOW                  
         BE    XIT                                                              
*                                                                               
         ZIC   R2,GLARGS+15        (USE PERIOD NUMBER)                          
         BCTR  R2,0                                                             
         SLL   R2,1                                                             
         L     RE,PDAFLBUF                                                      
         AR    R2,RE               (TO DISPLACE INTO DATE TABLE)                
         CLI   GLARGS+15,106       FOR 1-105,                                   
         BL    NOWEEK              USE REGULAR WEEK ROUTINES                    
         CLI   GLARGS+15,131       FOR 106-130                                  
         BL    NOMONTH             USE REGULAR MONTH ROUTINES                   
         CLI   GLARGS+15,139       FOR 131-138                                  
         BL    NOQUART             USE REGULAR QUARTER ROUTINES                 
         B     NOYEAR              ELSE USE YEAR                                
*                                                                               
NOWEEK   GOTO1 NETUNBK,DMCB,(C'W',0(R2)),DUB,GETDAY,ADDAY,GETBROAD              
         GOTO1 DATCON,DMCB,DUB,(8,0(R3))                                        
         B     XIT                                                              
*                                                                               
NOMONTH  L     RE,=A(MONTHTAB)                                                  
         ZIC   RF,1(R2)                                                         
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MH    RF,=H'4'                                                         
         AR    RE,RF                                                            
         MVC   1(4,R3),0(RE)       MOVE MONTH LITERAL                           
         EDIT  (1,(R2)),(2,5(R3))  MOVE YEAR OUT                                
         B     XIT                                                              
*                                                                               
NOQUART  LA    RE,QUARTTAB                                                      
         ZIC   RF,1(R2)                                                         
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MH    RF,=H'3'                                                         
         AR    RE,RF                                                            
         MVC   2(3,R3),0(RE)       MOVE QUARTER LITERAL                         
         EDIT  (1,(R2)),(2,5(R3))  MOVE YEAR OUT                                
         B     XIT                                                              
*                                                                               
NOYEAR   EDIT  (1,(R2)),(2,3(R3))  MOVE YEAR OUT                                
         B     XIT                                                              
*                                                                               
*--SEE IF ROOM IN HEADLINE FOR FILTER                                           
*HKROOM  LA    R6,4                                                             
CHKROOM  LA    R2,4                                                             
*                                                                               
CHKR02   CLC   0(8,R3),NDSPACES    IF ANYTHING IS THERE ALREADY                 
         BE    CHKR04                                                           
         LA    R3,198(R3)             BUMP DOWN A LINE                          
         BCT   R2,CHKR02                                                        
*                                                                               
CHKR04   BR    RE                  NO ROOM FOR FILTER                           
         EJECT                                                                  
* SHARED OUTPUT ROUTINE                                                         
* LABLAREA HAS PREFIX                                                           
* CODEAREA HAS CODE                                                             
* NAMEAREA HAS NAME                                                             
*                                                                               
GENOUT   L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         TM    GLINDS,X'40'        TEST TOTALS ROUTINE                          
         BO    TOTOUT              YES                                          
         CLI   MYLTYP,C'H'         FOR HEADLINES, MOVE OUT THE LOT              
         BNE   GENO02                                                           
         OC    OUTAREA,BLANKS                                                   
         MVC   0(L'OUTAREA,R3),OUTAREA                                          
         B     GENOX                                                            
*                                                                               
GENO02   OC    CODENNAM,BLANKS                                                  
         GOTO1 SQUASHER,DMCB,CODENNAM,59                                        
         LA    R1,CODENNAM         SET UP FOR CHOPPER                           
         ST    R1,DMCB             A(INPUT)                                     
         MVI   DMCB,59             L'INPUT                                      
         ST    R3,DMCB+4           A(OUTPUT)                                    
         MVC   DMCB+4(1),MYOLEN    LENGTH OF OUTPUT                             
         LA    R1,4                MAX N'LINES                                  
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,C'P'         SET P3 BYTE 1 TO C'P'                        
         CLI   WIDEOPT,C'Y'        UNLESS ITS WIDE                              
         BNE   *+8                                                              
         MVI   DMCB+8,198          WHEN PRINT LINES ARE 198 APART               
         GOTO1 CHOPPER,DMCB                                                     
         B     GENOX                                                            
*                                                                               
GENOX    B     XIT                                                              
         EJECT                                                                  
* SHARED TOTAL ROUTINE                                                          
*                                                                               
TOTOUT   DS    0H                                                               
         ZIC   R4,ROW1WIDE                                                      
         BCTR  R4,0                                                             
         CLC   LABLAREA,BLANKS                                                  
         BNH   TOTO02                                                           
         CLI   ROWWIDTH,8                                                       
         BL    TOTO02                                                           
         LA    RF,L'LABLAREA                                                    
         LA    R1,LABLAREA+L'LABLAREA-1                                         
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         LA    RF,1(RF)                                                         
         CR    RF,R4                                                            
         BNH   TOTO04                                                           
         ZIC   R4,ROWWIDTH                                                      
         BCTR  R4,0                                                             
         CR    RF,R4                                                            
         BNH   TOTO04                                                           
*                                                                               
TOTO02   CH    R4,=H'3'                                                         
         BL    TOTOX                                                            
         MVC   0(3,R3),=C'ALL'                                                  
         CH    R4,=H'5'                                                         
         BL    TOTOX                                                            
         MVC   0(5,R3),=C'*ALL*'                                                
         CH    R4,=H'8'                                                         
         BL    TOTOX                                                            
         MVC   0(9,R3),=C'*AVERAGE*'                                            
         B     TOTOX                                                            
*                                                                               
TOTO04   MVC   BLOCK(80),BLANKS                                                 
         MVI   BLOCK,C'*'                                                       
         MVC   BLOCK+1(15),LABLAREA                                             
         MVC   BLOCK+17(8),=C'AVERAGE*'                                         
         GOTO1 SQUASHER,DMCB,BLOCK,80                                           
         LA    R1,2                                                             
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198                                                       
         GOTO1 CHOPPER,DMCB,(80,BLOCK),((R4),0(R3))                             
*                                                                               
TOTOX    MVC   OUTAREA,BLANKS                                                   
         B     XIT                                                              
         EJECT                                                                  
* ABOUT TO PASS DRIVER RECORD TO THE SORT                                       
*                                                                               
PUTSRT   DS    0H                                                               
         OC    LEVELSWS,LEVELSWS   TEST ALREADY WANT TO SUPPRESS                
         BZ    PUTSX                                                            
         LA    RE,L'LEVELSWS                                                    
         LA    RF,LEVELSWS                                                      
         CLI   0(RF),C'Y'                                                       
         BE    PUTSX                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
*                                                                               
         MVI   GLHOOK,GLDONT                                                    
*                                                                               
PUTSX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
WB1TCRF  EQU   181                                                              
WB1TCRL  EQU   190                                                              
*                                                                               
NADUHED  DS    0CL3                                                             
         DC    C'A',C'IM'          2ND LINE OF HEADING FOR NAD(U)               
         DC    C'B',C'OP'                                                       
         DC    C'C',C'FT'                                                       
         DC    C'I',C'FT'                                                       
         DC    C'T',C'FT'                                                       
         DC    C'X',C'FT'                                                       
         DC    X'AB',C'FT'                                                      
         DC    X'FF'                                                            
*                                                                               
*--DATE TABLES                                                                  
NADQUART DC    C'Q1/ ',X'01',AL1(1,3,0,0)       QUARTERS                        
         DC    C'Q2/ ',X'02',AL1(4,6,0,0)                                       
         DC    C'Q3/ ',X'03',AL1(7,9,0,0)                                       
         DC    C'Q4/ ',X'04',AL1(10,12,0,0)                                     
*                                                                               
QUARTTAB DC    C'Q1/'                                                           
         DC    C'Q2/'                                                           
         DC    C'Q3/'                                                           
         DC    C'Q4/'                                                           
*                                                                               
IVTYTAB  DS    0X                    VIEWING TYPES                              
*DBLOCK VALUE(1), OUTPUT CHARACTER (1)                                          
         DC    AL1(DBXLLQ),C'L'      LIVE                                       
         DC    AL1(DBXLL1Q),C'S'     LIVE+SD                                    
         DC    AL1(DBXLL7Q),C'7'     LIVE+7                                     
         DC    AL1(DBXLLF1Q),C'1'    LIVE+1                                     
         DC    AL1(DBXLL2Q),C'2'     LIVE+2                                     
         DC    AL1(DBXLL3Q),C'3'     LIVE+3                                     
         DC    AL1(DBXLALVQ),C'A'    LIVE    FROM ACM TAPES                     
         DC    AL1(DBXLALSQ),C'B'    LIVE+SD FROM ACM TAPES                     
         DC    AL1(DBXLAL7Q),C'C'    LIVE+7  FROM ACM TAPES                     
         DC    X'FF'                                                            
*                                                                               
SRTVTYP  DS    0X                    SORT SEQUENCE FOR VIEWING TYPES            
*DBLOCK VALUE(1), OUTPUT CHARACTER (1)                                          
         DC    AL1(SRCLIVE),AL1(1)                                              
         DC    AL1(SRCLIVSD),AL1(2)                                             
         DC    AL1(SRCLIVE1),AL1(3)                                             
         DC    AL1(SRCLIVE2),AL1(4)                                             
         DC    AL1(SRCLIVE3),AL1(5)                                             
         DC    AL1(SRCLIVE7),AL1(6)                                             
         DC    AL1(SRCALV),AL1(7)                                               
         DC    AL1(SRCALS),AL1(8)                                               
         DC    AL1(SRCAL7),AL1(9)                                               
         DC    X'FF'                                                            
*                                                                               
B4       DC    X'03',XL4'00000000'                                              
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
BLANKS   DS    0CL80                                                            
NDSPACES DC    CL198' '                                                         
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEPODPREC                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
*                                                                               
FLWNAD   DC    X'01',AL1(1,2,3,0,0,0,0,0,0,0,0,0,0,0)                           
         DC    X'02',AL1(4,5,6,0,0,0,0,0,0,0,0,0,0,0)                           
         DC    X'03',AL1(7,8,9,0,0,0,0,0,0,0,0,0,0,0)                           
         DC    X'04',AL1(10,11,12,0,0,0,0,0,0,0,0,0,0,0)                        
         DC    X'FF'                                                            
*                                                                               
MONTHTAB DC    C'JAN/'                                                          
         DC    C'FEB/'                                                          
         DC    C'MAR/'                                                          
         DC    C'APR/'                                                          
         DC    C'MAY/'                                                          
         DC    C'JUN/'                                                          
         DC    C'JUL/'                                                          
         DC    C'AUG/'                                                          
         DC    C'SEP/'                                                          
         DC    C'OCT/'                                                          
         DC    C'NOV/'                                                          
         DC    C'DEC/'                                                          
FILTDPTB DC    C'E',X'01',X'7C02BC03E70000000000',C'EARLY'                      
*                            M-F,7-959A                                         
         DC    C'D',X'01',X'7C03E8065D0000000000',C'DAY  '                      
*                            M-F,10A-429P                                       
         DC    C'P',X'02',X'FF07D008FB01076C07D0',C'PRIME'                      
*                            ALL,8-1059P SUN,7-8P                               
         DC    C'L',X'02',X'7C091A0ABC7C0000012B',C'LATE '                      
*                            M-F,1130P-259A                                     
         DC    C'S',X'01',X'02032005130000000000',C'SATAM'                      
*                            SAT,8A-1259P                                       
         DC    C'W',X'02',X'0202BC076B0102BC076B',C'WKEND'                      
*                            SAT,7A-659P SUN,7A-659P                            
         DC    C'N',X'01',X'FF0708076B0000000000',C'NEWS '                      
*                            ALL,6-659P                                         
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
*                                                                               
* FOR CNAD/CNAW, REPORT BOU/GRT/ESC/LAF  (SPEC-24119)                           
*                REPORT MYS              (SPEC-49924)                           
*                                                                               
TSTCNA   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOU ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ESC ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'GRT ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'LAF ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ION ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'HI  ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MET ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'STV ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MYS ',PDNET                                                   
         JNE   NO                                                               
*                                                                               
         TM    PODFLAG,PODCNADQ+PODCNAWQ                                        
         JZ    NO                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
* FOR CNAD/CNAW, REPORT BOU/GRT/ESC/LAF  (SPEC-24119)                           
*                REPORT MYS              (SPEC-49924)                           
*                                                                               
SETCNA   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOU ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ESC ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'GRT ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'LAF ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ION ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'HI  ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MET ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'STV ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MYS ',PDNET                                                   
         JNE   SCNAX                                                            
*                                                                               
         CLC   =C'NAD ',PDSOURCE                                                
         JE    SCNA04                                                           
         CLC   =C'NAW ',PDSOURCE                                                
         JE    SCNA04                                                           
*                                                                               
         CLC   =C'CNAD',PDSOURCE                                                
         JNE   SCNA02                                                           
         MVC   PDSOURCE(4),=C'NAD '                                             
         J     SCNA04                                                           
*                                                                               
SCNA02   CLC   =C'CNAW',PDSOURCE                                                
         JNE   SCNAX                                                            
         MVC   PDSOURCE(4),=C'NAW '                                             
*                                                                               
SCNA04   CLI   PDMETHOD,4          METHOD = SOURCE                              
         JE    SCNAX                                                            
         MVI   PDBASE,C'R'                                                      
         MVI   PDDUROPT,0                                                       
*                                                                               
SCNAX    J     XIT                                                              
         LTORG                                                                  
*                                                                               
* FOR CNAD/CNAW, REPORT BOU/GRT/ESC/LAF  (SPEC-24119)                           
*                REPORT MYS              (SPEC-49924)                           
*                                                                               
RESTCNA  NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOU ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ESC ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'GRT ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'LAF ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ION ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'HI  ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MET ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'STV ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MYS ',PDNET                                                   
         JNE   RCNAX                                                            
*                                                                               
         TM    PODFLAG,PODCNADQ                                                 
         JZ    RCNA02                                                           
         CLC   =C'NAD ',PDSOURCE   SET TO NAD IN SETCNA ROUTINE                 
         JNE   RCNA02                                                           
         MVC   PDSOURCE(4),=C'CNAD'                                             
         J     RCNA04                                                           
*                                                                               
RCNA02   TM    PODFLAG,PODCNAWQ                                                 
         JZ    RCNAX                                                            
         CLC   =C'NAW ',PDSOURCE   SET TO NAW IN SETCNA ROUTINE                 
         JNE   RCNAX                                                            
         MVC   PDSOURCE(4),=C'CNAW'                                             
*                                                                               
RCNA04   CLI   PDMETHOD,4          METHOD = SOURCE                              
         JE    RCNAX                                                            
         MVI   PDBASE,C'I'                                                      
         MVI   PDDUROPT,C'M'                                                    
*                                                                               
RCNAX    J     XIT                                                              
*                                                                               
***********************************************************************         
*-- CONVERTS YY/WW INTO 6 BYTE DATE                                             
*--P1=A(BINARY Y/W)                                                             
*--P2=A(6-BYTE DATE)                                                            
***********************************************************************         
UNWEEK   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
*                                                                               
         ZIC   R1,0(R2)            GENERATE JAN01 OF SPECIFIED YEAR             
         EDIT  (R1),(2,0(R3))                                                   
         OI    0(R3),X'F0'                                                      
         MVC   2(4,R3),=C'0101'                                                 
         GOTOR GETDAY,DMCB,0(R3),DMCB+12                                        
         ZIC   R0,0(R1)            BACK UP TO PREVIOUS MONDAY                   
         LA    R4,1                                                             
         SR    R4,R0                                                            
         GOTOR ADDAY,DMCB,0(R3),DMCB+12,(R4)                                    
         ZIC   R5,1(R2)                                                         
*                                                                               
         BCTR  R5,0                                                             
         MH    R5,=H'7'                                                         
         ST    R5,DMCB+8                                                        
         GOTOR ADDAY,DMCB,DMCB+12,0(R3)                                         
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTNET - ROUTINE TO FILTER UNITS                                             
* ARGUMENTS              +6         7 FILTER                                    
*                        +7         8 NETWORK                                   
*                        +8         9 DAY                                       
*                        +9        10 DAYPART                                   
*                        +10       11 SOURCE                                    
*                        +11       12 X'80'OPTION NOT TO TOTAL THIS COL         
*                        +12       13 BOOK                                      
*                        +13       14 VIEWING TYPE                              
*                        +15       16 FLOWCHART                                 
***********************************************************************         
FILTNET  NTR1  BASE=*,LABEL=*                                                   
         CLI   GLARGS+6,0                                                       
         BE    FILF09                                                           
         CLI   GLARGS+6,1                                                       
         BNE   FILF02                                                           
         CLI   PDFILT+3,C'O'                                                    
         BNE   FILTNO                                                           
         B     FILF09                                                           
FILF02   CLI   GLARGS+6,2                                                       
         BNE   FILF09                                                           
         CLI   PDFILT+3,C'R'                                                    
         BNE   FILTNO                                                           
         B     FILF09                                                           
FILF09   DS    0H                                                               
*                                                                               
         CLI   GLARGS+7,0          NETWORK FILTER                               
         BE    FILT06                                                           
*                                                                               
         ZIC   RE,GLARGS+7                                                      
         BCTR  RE,0                                                             
         L     R1,APODNET                                                       
         CLI   GLARGS+7,1                                                       
         BE    FILT02                                                           
*                                                                               
         LA    R1,PODNETL(R1)                                                   
         BCT   RE,*-4                                                           
*                                                                               
FILT02   CLC   PDNET(6),0(R1)      PODNET DOESNT HAVE MRKT                      
         BNE   FILT04                                                           
         B     FILT06                                                           
*                                                                               
FILT04   CLC   =C'ZZZ',0(R1)       IS IT ALL NETS                               
         BNE   FILTNO                                                           
         CLC   PDBKTYP,5(R1)       COMPARE BOOK TYPES                           
         BNE   FILTNO              NOT EQUAL, CAN'T USE IT                      
*                                                                               
FILT06   CLI   GLARGS+12,0                                                      
         BZ    FILT16                                                           
         L     R1,APODBKL                                                       
         CLI   GLARGS+12,1                                                      
         BE    FILT12                                                           
         ZIC   RE,GLARGS+12                                                     
         BCTR  RE,0                                                             
*                                                                               
* FIND THE CORRESPONDING BOOK                                                   
FILT08   CLI   0(R1),1                                                          
         BNE   FILT10                                                           
         LA    R1,PODBLNQ(R1)                                                   
*                                                                               
FILT10   LA    R1,PODBLNQ(R1)                                                   
         BCT   RE,FILT08                                                        
*                                                                               
FILT12   CLI   0(R1),0             ONE BOOK ONLY?                               
         BNE   FILT14              NO, COULD BE A RANGE.                        
         CLC   PDSTYPE(1),15(R1)   COMPARE STATION TYPES                        
         BNE   FILTNO               NE, CAN'T USE IT                            
         CLC   PDBKTYP(1),14(R1)   COMPARE BOOK TYPES                           
         BNE   FILTNO               NE, CAN'T USE IT                            
         CLC   PDSBOOK(2),12(R1)   START BOOK SAME?                             
         BNE   FILTNO                                                           
         B     FILT16                                                           
*                                                                               
FILT14   CLC   PDSBOOK(2),12(R1)   GREATER THAN OR EQUAL START BOOK?            
         BL    FILTNO                                                           
         CLC   PDSBOOK(2),PODBLNQ+12(R1)   LTE END BOOK?                        
         BH    FILTNO                                                           
         CLC   PDSBOOK+2(2),PODBLNQ+12(R1) END BOOK SAME?                       
         BNE   FILTNO                                                           
         B     FILT16                                                           
*                                                                               
FILT16   CLI   GLARGS+8,0          DAY FILTER                                   
         BE    FILT18                                                           
*                                                                               
         CLC   PDDAY(1),GLARGS+8                                                
         BNE   FILTNO                                                           
         B     FILT18                                                           
*                                                                               
FILT18   CLI   GLARGS+9,0          DAYPART FILTER                               
         BE    FILT28                                                           
         L     R2,=A(FILTDPTB)                                                  
*                                                                               
FILT20   CLC   0(1,R2),GLARGS+9                                                 
         BE    FILT22                                                           
         LA    R2,17(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   FILT20                                                           
         DC    H'0'                                                             
*                                                                               
FILT22   ZIC   RE,1(R2)                                                         
         LA    R2,2(R2)                                                         
*                                                                               
FILT24   MVC   BYTE,0(R2)          MOVE DAY                                     
         NC    BYTE,PDDAY+1        CHECK FOR DAY MATCH                          
         BZ    FILT26              NO BYPASS                                    
         CLC   1(2,R2),PDTIME                                                   
         BH    FILT26                                                           
         CLC   3(2,R2),PDTIME                                                   
         BNL   FILT28                                                           
*                                                                               
FILT26   LA    R2,5(R2)                                                         
         BCT   RE,FILT24                                                        
         B     FILTNO                                                           
*                                                                               
FILT28   DS    0H                                                               
         CLI   GLARGS+10,0         SOURCE FILTER                                
         BE    FILT33                                                           
         L     R2,=A(FILTSRTB)                                                  
*                                                                               
FILT30   CLC   0(1,R2),GLARGS+10                                                
         BE    FILT32                                                           
         LA    R2,11(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   FILT30                                                           
         DC    H'0'                                                             
*                                                                               
FILT32   CLC   PDSOURCE,1(R2)      CHECK SOURCE MATCH                           
         BE    FILT33                                                           
         CLC   PDSOURCE,6(R2)      CHECK SOURCE MATCH                           
         BE    FILT33                                                           
         B     FILTNO              NO BYPASS                                    
*                                                                               
FILT33   CLI   GLARGS+13,0         VIEWING TYPE FILTER                          
         BE    FILT34                                                           
         CLC   PDACTVIW,GLARGS+13                                               
         BE    FILT34                                                           
         B     FILTNO                                                           
*                                                                               
FILT34   CLI   GLARGS+14,0         MINUTE TYPE FILTER                           
         BE    FILT35                                                           
         TM    GLARGS+14,OPTMCOMQ  COMMERCIAL MINUTE REQUESTED                  
         BNO   *+12                                                             
         CLI   PDCOMFDF,C'Y'       CK COMMERCIAL SECONDS                        
         BE    FILT35                                                           
         TM    GLARGS+14,OPTMPROQ  PROMO MINUTE REQUESTED                       
         BNO   *+12                                                             
         CLI   PDMPROSC,0          CK PROMO SECONDS                             
         BH    FILT35                                                           
         TM    GLARGS+14,OPTMPSAQ  COMMERCIAL MINUTE REQUESTED                  
         BNO   FILTNO                                                           
         CLI   PDMPSASC,0          CK COMMERCIAL SECONDS                        
         BNH   FILTNO                                                           
*                                                                               
FILT35   CLI   GLARGS+15,0        FLOWCHART FILTER                              
         BE    FILTYES                                                          
         ZIC   R2,GLARGS+15                                                     
         BCTR  R2,0                                                             
         SLL   R2,1                                                             
         L     RE,PDAFLBUF                                                      
         AR    R2,RE               (TO DISPLACE INTO DATE TABLE)                
         CLI   GLARGS+15,106       FOR 1-105,                                   
         BL    FILT36              USE REGULAR WEEK ROUTINES                    
         CLI   GLARGS+15,131       FOR 106-130                                  
         BL    FILT38              USE REGULAR MONTH ROUTINES                   
         CLI   GLARGS+15,139       FOR 131-138                                  
         BL    FILT48              USE REGULAR QUARTER ROUTINES                 
         B     FILT62              ELSE USE YEAR                                
*                                                                               
*--WEEKLY LEVEL                                                                 
FILT36   CLC   PDSBOOK(2),0(R2)    CHECK DATES EQUAL                            
         BE    FILTYES                                                          
         B     FILTNO                                                           
*                                                                               
*--MONTHLY LEVEL                                                                
FILT38   TM    PODNADBK,X'F0'      FOR NAD NO MONTHLY CONV REQUIRED             
         BO    *+8                   OR FOR SPOT SOURCES                        
         B     FILT40                                                           
         CLC   =C'PIV',PDSOURCE    EXCEPTIONAL NAD SOURCE                       
         BE    FILT40                                                           
         CLC   PDSBOOK(2),0(R2)    CHECK DATES EQUAL                            
         BE    FILTYES                                                          
         B     FILTNO                                                           
*                                                                               
FILT40   CLI   PDCALOPT,PDCALNTI   SPECIAL CALENDAR                             
         BE    FILT44                                                           
*                                                                               
FILT42   CLC   PDSBOOK(1),0(R2)    CHECK YEAR EQUAL                             
         BNE   FILTNO                                                           
         GOTOR NETUNBK,DMCB,(C'M',PDSBOOK),DUB,GETDAY,ADDAY,GETBROAD            
         PACK  DUB(8),DUB+2(2)                                                  
         CVB   R1,DUB                                                           
         STC   R1,DUB                                                           
         CLC   DUB(1),1(R2)        CHECK IF MONTH'S EQUAL                       
         BE    FILTYES                                                          
         B     FILTNO                                                           
*                                                                               
* MONTHS FOR CALENDAR=NTI OPTION                                                
FILT44   GOTOR UNWEEK,DMCB,PDSBOOK,DUB                                          
         GOTOR DATCON,DMCB,(0,DUB),(3,FULL)                                     
         MVC   DUB(3),FULL                                                      
*                                  CALENDARIZE NTI MONTHS                       
         L     RF,ACOMFACS         GET A(MONTH/DATES TABLE)                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,MTHDATES                                               
         ICM   RE,15,0(R1)         A(MONTH/DATES TABLE)                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            LENGTH OF TABLE ENTRY                        
*                                                                               
         USING MTHDATD,RE                                                       
         CLC   DUB(3),MDSDATE      NOT IN TABLE                                 
         BL    FILT42                                                           
*                                                                               
FILT46   CLI   MDNLSMN,13                                                       
         BE    FILT42                                                           
         CLC   DUB(3),MDEDATE                                                   
         BNH   *+10                                                             
         AR    RE,RF                                                            
         B     FILT46                                                           
         CLC   MDNLBOOK,0(R2)      IS THIS THE CORRECT SLOT                     
         BE    FILTYES                                                          
         B     FILTNO                                                           
         DROP  RE                                                               
*                                                                               
*--QUARTERLY LEVEL                                                              
FILT48   CLI   PDCALOPT,PDCALNTI   SPECIAL CALENDAR OPTION                      
         BE    FILT54                                                           
*                                                                               
FILT50   CLC   PDSBOOK(1),0(R2)    CHECK YEAR EQUAL                             
         BNE   FILTNO                                                           
         TM    PODNADBK,X'F0'      CHECK FOR NAD AND SPOT                       
         BO    FILT58                                                           
         GOTOR NETUNBK,DMCB,(C'Q',PDSBOOK),DUB,GETDAY,ADDAY,GETBROAD            
         MVC   DUB(1),DUB+1                                                     
*                                                                               
FILT52   NI    DUB,X'0F'                                                        
         CLC   DUB(1),1(R2)                                                     
         BE    FILTYES                                                          
         B     FILTNO                                                           
*                                                                               
* QUARTERS FOR CALENDAR=NTI                                                     
FILT54   GOTOR UNWEEK,DMCB,PDSBOOK,DUB                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NENTIQRT  GET NIELSEN QUARTER TABLE                    
         ICM   RE,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NEQTRD,RE                                                        
         CLC   DUB(6),NEQTRYR      NOT IN TABLE                                 
         BL    FILT50                                                           
*                                                                               
FILT56   CLI   0(RE),X'FF'                                                      
         BE    FILT50                                                           
         CLC   DUB(6),NEQTRLST     FIND NTI QUARTER                             
         BNH   *+12                                                             
*** WHOEVER WORKS ON THIS MODULE NEXT: GET RID OF THIS REFERENCE TO             
*** NEQTRQ !!! USE THE SOFT LENGTH RETURNED FROM DEMTABS INSTEAD !!!            
         LA    RE,NEQTRQ(RE)                                                    
         B     FILT56                                                           
*                                                                               
         LR    R1,RE               ADJUST DATA FORMATS                          
         XC    DUB,DUB                                                          
         PACK  DUB+6(2),NEQTRYR                                                 
         CVB   RE,DUB                                                           
         CH    RE,=H'27'                                                        
         BH    *+8                                                              
         LA    RE,100(RE)                                                       
         STC   RE,DUB                                                           
         CLC   DUB(1),0(R2)        SAME YEAR                                    
         BNE   FILTNO                                                           
         MVC   DUB(1),3(R1)        SET QUARTER                                  
         B     FILT52               AND FINISH UP                               
         DROP  RE                                                               
*                                                                               
*--QUARTERLY LEVEL (FOR NAD)                                                    
FILT58   L     RE,=A(FLWNAD)           NAD QUARTER TABLE                        
         ZIC   RF,1(R2)                                                         
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MH    RF,=H'15'                                                        
         AR    RE,RF                                                            
         LA    RF,14                                                            
*                                                                               
FILT60   CLC   PDSBOOK+1(1),1(RE)  CHECK IF QUARTERS EQUAL                      
         BE    FILTYES                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,FILT60                                                        
         B     FILTNO                                                           
*                                                                               
*--YEARLY LEVEL                                                                 
FILT62   CLC   PDSBOOK(1),0(R2)    CHECK DATES EQUAL                            
         BE    FILTYES                                                          
         B     FILTNO                                                           
*                                                                               
FILTYES  SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
FILTNO   LA    R1,1                                                             
         LTR   R1,R1                                                            
*                                                                               
FILTX    J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INPUT ROUTINE TO PROCESS DEMO VALUES                                          
***********************************************************************         
IDMS     NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETCNA           REPORT CERTAIN STATIONS IF CNAD/CNAW         
*                                                                               
         L     R4,PDADEMTB         SET UP DEMO BUFFER                           
         USING PDDMBUFF,R4                                                      
         ZIC   RE,GLARGS          DEMO OFFSET                                   
         ZIC   R1,PODDMENT        NUMBER OF DEMOS IN A SERIES                   
         MH    R1,=H'4'           CALC. LENGTH BETWEEN EACH DEMO GROUP          
         LA    RF,PDDEMOS                                                       
         CLI   GLARGS,0                                                         
         BE    *+10                                                             
         AR    RF,R1                                                            
         BCT   RE,*-2                                                           
*                                                                               
         LA    RE,PDDEMOS          SAVE ADDR FOR IMP AVERAGING OPT              
         LR    R1,RF                                                            
         SR    R1,RE                                                            
*                                                                               
         MVC   DUB(4),PDWEIGHT                                                  
         CLC   PDSOURCE(5),=C'ACMWB' MKT WEIGHT FOR ACMWB ONLY                  
         BNE   IDMS01                                                           
         TM    OPTFLAG1,ACMMWTQ     & ONLY IF WEIGHTED BY COM SECONDS           
         BO    IDMS01                                                           
         LA    R5,PODDMWGT                                                      
         AR    R5,R1                                                            
         MVC   PDWEIGHT,0(R5)                                                   
IDMS01   A     R1,APDD2VAL                                                      
         MVC   FULL,0(R1)                                                       
*                                                                               
         TM    0(RF),X'80'         NEGATIVE INDICATES NOT IN THIS PASS          
         BZ    IDMSVAL                                                          
         ZAP   0(8,R2),=PL1'0'     SO ZAP ALL ACCUMULATORS AND EXIT             
         MVC   8(24,R2),0(R2)                                                   
         B     IDMS10              NEED THIS FOR SOLO=N                         
*                                                                               
IDMSVAL  ICM   R1,15,0(RF)                                                      
         CVD   R1,0(R2)                                                         
         ZAP   8(8,R2),=PL1'0'     SET WEIGHT TO ZERO                           
         OC    0(4,RF),0(RF)       IF DEMO = TO ZERO DONT SET WEIGHT            
         BNZ   IDMS04              NEED WEIGHT FOR AVERAGING                    
         CLC   =C'NAD',PDSOURCE    MULTIPLE READ??  (NAD ESPECIALLY)            
         BE    IDMS02                SKIP WEIGHT 2ND+ TIME AROUND               
         CLC   =C'NAW',PDSOURCE                                                 
         BE    IDMS02                                                           
         CLC   =C'NHT',PDSOURCE                                                 
         BE    IDMS02                                                           
         CLC   =C'HPM',PDSOURCE                                                 
         BE    IDMS02                                                           
         CLC   =C'NHW',PDSOURCE                                                 
         BE    IDMS02                                                           
         CLC   =C'HPW',PDSOURCE                                                 
         BE    IDMS02                                                           
         OC    PDWEIGHT,PDWEIGHT   WEIGHT ALSO HAS TO BE ZERO                   
         BNZ   IDMS04                                                           
*                                                                               
IDMS02   CLI   PDNET+4,C'C'        CABLE?                                       
         BNE   IDMS06              CABLE WILL USE ZERO DEMO                     
*                                                                               
IDMS04   ICM   R1,15,PDWEIGHT      GET THE WEIGHTING FACTOR                     
         CVD   R1,8(R2)                                                         
         ZAP   WORK(16),=PL1'0'                                                 
         MVC   WORK+8(8),0(R2)                                                  
         MP    WORK(16),8(8,R2)    MULT DEMO BY WEGHT                           
         MVC   0(8,R2),WORK+8                                                   
         CLI   PDMETHOD,5          METHOD=NSI                                   
         BNE   IDMS04C                                                          
         ZIC   R1,GLARGS                                                        
         MHI   R1,3                                                             
         AR    R1,R4                                                            
         CLI   1(R1),C'R'                                                       
         BE    *+8                                                              
         CLI   1(R1),C'P'                                                       
         BE    *+8                                                              
         CLI   1(R1),C'S'                                                       
         BNE   IDMS05                                                           
         CLI   2(R1),2             METROS DON'T NEED ADJUSTMENT                 
         BE    IDMS05                                                           
         CLI   2(R1),3                                                          
         BE    IDMS05                                                           
         ZAP   WORK(16),=PL1'0'                                                 
         MVC   WORK+8(8),0(R2)                                                  
         CLI   1(R1),C'S'                                                       
         BNE   IDMS04A                                                          
         CLI   2(R1),1             HOME SHARES DON'T NEED ADJUSTMENT            
         BE    IDMS04B                                                          
         MP    WORK(16),=PL8'1000' *1000 FOR S                                  
         B     *+10                                                             
IDMS04A  MP    WORK(16),=PL8'10'   *10 FOR R,P                                  
IDMS04B  MVC   0(8,R2),WORK+8                                                   
         B     IDMS05                                                           
*                                                                               
IDMS04C  CLI   PDBASE,C'B'                                                      
         BE    *+8                                                              
         CLI   PDBASE,C'I'                                                      
         BNE   IDMS06                                                           
IDMS05   ICM   R1,15,FULL          STILL NEED WEIGHT IF VALUE ZERO              
         BNZ   *+8                                                              
         LA    R1,1                                                             
         ZAP   WORK(16),=PL1'0'                                                 
         CVD   R1,WORK+8                                                        
         MP    WORK(16),8(8,R2)                                                 
         MVC   8(8,R2),WORK+8                                                   
*&&DO                                                                           
* COMMENT THIS OUT WHEN WE WANT NEW AVERAGING LOGIC                             
* EXCLUDE ZERO ENTRIES FOR P1,Z1,S1 IF YHOMES NOT ZERO                          
* THIS LOGIC DOESN'T WORK WITH STACKED DEMOS                                    
* IT ONLY APPLIES TO METHOD=SOURCE AND METHOD=AE                                
         CLC   =C'NTI',PDSOURCE    FOR BROADCAST NETWK AND SYNDICATION          
         BE    *+14                                                             
         CLC   =C'ACM',PDSOURCE    AND COMMERCIAL AVERAGE                       
         BNE   IDMS05A                                                          
         CLI   PDNET+4,C'T'                                                     
         BE    *+12                                                             
         CLI   PDNET+4,C'S'                                                     
         BNE   IDMS05A                                                          
         ZIC   R1,GLARGS                                                        
         MH    R1,LNDEMCOD                                                      
         AR    R1,R4                                                            
         CLI   1(R1),C'P'          FOR HOMES PUTS,SHARES, AND PROG PUTS         
         BE    *+8                                                              
         CLI   1(R1),C'S'                                                       
         BE    *+8                                                              
         CLI   1(R1),C'Z'                                                       
         BNE   IDMS05A                                                          
         CLC   LNDEMCOD,=H'3'                                                   
         BNE   IDMS05A4                                                         
         CLI   2(R1),1             HOMES FOR 3-BYTE DEMOS                       
         BNE   IDMS05A                                                          
         B     IDMS05A5                                                         
IDMS05A4 CLC   =H'1',2(R1)         HOMES FOR 4-BYTE DEMOS                       
         BNE   IDMS05A                                                          
IDMS05A5 CLC   0(8,R2),=PL8'0'         IF NUMERATOR IS ZERO                     
         BNE   IDMS05A                                                          
         OC    PDHOMES,PDHOMES         AND HOMES NUMBERS ARE AVAILABLE          
         BZ    IDMS05A                                                          
         MVC   8(8,R2),=PL8'0'         DON'T INCLUDE IN THE AVERAGE             
         CLI   PDMETHOD,6              FOR METHOD=AE                            
         BNE   IDMS06                                                           
         MVC   16(8,R2),=PL8'0'        DON'T ACCUMULATE WEIGHT                  
         B     IDMS08                                                           
*&&                                                                             
IDMS05A  CLI   PDMETHOD,6              FOR METHOD=AE                            
         BNE   IDMS06                                                           
         ICM   RF,15,PDWEIGHT                                                   
         CVD   RF,16(R2)               CUMMULATE TOTAL WEIGHT HERE              
         B     IDMS08                                                           
*                                                                               
*-----SET SOURCE INDICATOR                                                      
IDMS06   CLI   PDMETHOD,6              SKIP SOURCE FOR METHOD=AE                
         BE    IDMS08                                                           
         CLC   PDSOURCE(3),=CL3'NAD'                                            
         BNE   *+14                                                             
         ZAP   16(8,R2),=PL1'1'                                                 
         B     IDMS08                                                           
         CLC   PDSOURCE(3),=CL3'PIV'                                            
         BNE   *+14                                                             
         ZAP   16(8,R2),=PL1'-1'                                                
         B     IDMS08                                                           
         ZAP   16(8,R2),=PL1'0'                                                 
*                                                                               
*-----SET MEDIA INDICATOR                                                       
IDMS08   CLI   PDMEDIA,C'C'                                                     
         BNE   *+14                                                             
         ZAP   24(8,R2),=PL1'1'                                                 
         B     IDMS10                                                           
         CLI   PDMEDIA,C'S'                                                     
         BNE   *+14                                                             
         ZAP   24(8,R2),=PL1'0'                                                 
         B     IDMS10                                                           
         ZAP   24(8,R2),=PL1'0'                                                 
*                                                                               
*-----SET TIMES RUN                                                             
IDMS10   MVC   24(4,R2),PDRUN                                                   
         MVC   PDWEIGHT,DUB                                                     
*                                                                               
         BRAS  RE,TSTCNA           TEST CERTAIN NETS IF CNAD/CNAW               
         JNE   IDMSX                                                            
         ZAP   32(8,R2),=PL1'1'    SET INDICATOR IT'S SPECIAL NET               
*                                                                               
IDMSX    BRAS  RE,RESTCNA          REPORT CERTAIN STATIONS IF CNAD/CNAW         
         XIT1  ,                                                                
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* OUTPUT ROUTINE TO PROCESS DEMO VALUES                                         
*                                                                               
**********************************************************************          
ODMS     NTR1  BASE=*,LABEL=*                                                   
         L     R4,PDADEMTB         SET UP DEMO BUFFER                           
         USING PDDMBUFF,R4                                                      
         CLI   GLHOOK,GLINCOMP                                                  
         BNE   ODMS08                                                           
         ZIC   RE,GLARGS          DEMO OFFSET                                   
         ZIC   R1,PODDMENT        NUMBER OF DEMOS IN A SERIES                   
         MH    R1,LNDEMCOD                                                      
         LA    RF,PODDEMO                                                       
         CLI   GLARGS,0                                                         
         BE    *+10                                                             
         AR    RF,R1                                                            
         BCT   RE,*-2                                                           
*                                                                               
ODMSAE   CLI   PDMETHOD,6          METHOD=AE,UNWEIGHT BEFORE DIVISION           
         BNE   ODMSAEX                                                          
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)     FIRST DEMO                                   
         OC    16(8,R2),16(R2)                                                  
         BZ    ODMSAE05                                                         
         CP    16(8,R2),=PL8'0'                                                 
         BNE   *+10                                                             
ODMSAE05 MVC   16(8,R2),=PL8'1'                                                 
         DP    WORK(16),16(8,R2)   DIVIDE FIRST DEMO BY WEIGHT                  
         MVC   0(8,R2),WORK                                                     
         XC    WORK(8),WORK                                                     
         MP    WORK(16),=PL1'2'    AND ROUND                                    
         CP    WORK(16),16(8,R2)                                                
         BL    *+10                                                             
         AP    0(8,R2),=PL1'1'                                                  
                                                                                
         XC    WORK(8),WORK                                                     
         MVC   WORK+8(8),8(R2)     SECOND DEMO                                  
         DP    WORK(16),16(8,R2)   DIVIDE SECOND DEMO BY WEIGHT                 
         MVC   8(8,R2),WORK                                                     
         XC    WORK(8),WORK                                                     
         MP    WORK(16),=PL1'2'    AND ROUND                                    
         CP    WORK(16),16(8,R2)                                                
         BL    *+10                                                             
         AP    8(8,R2),=PL1'1'                                                  
                                                                                
ODMSAEX  CP    32(8,R2),=PL8'0'    CHECK IF SPECIAL NET CNAD/CNAW               
         JNE   ODMS01                                                           
*                                                                               
         CLI   PDBASE,C'B'                                                      
         BNE   ODMS01                                                           
         CLI   1(RF),C'S'                                                       
         BE    ODMS04                                                           
*                                                                               
ODMS01   CLI   PDMETHOD,4          METHOD=SOURCE                                
         BE    ODMS01A                                                          
         CP    32(8,R2),=PL8'0'    CHECK IF SPECIAL NET CNAD/CNAW               
         JNE   ODMS02                                                           
         CLI   PDDUROPT,C'M'                                                    
         BNE   ODMS02                                                           
*                                                                               
ODMS01A  CP    8(8,R2),=PL8'0'     CHECK FOR ZERO DEMO                          
         BE    ODMSX                                                            
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)     DEMO AMOUNT                                  
         CLI   1(RF),C'N'          GAA IMPRESSIONS                              
         BE    *+8                                                              
         CLI   1(RF),C'H'          CABLE IMPRESSIONS                            
         BE    *+8                                                              
         CLI   1(RF),C'T'          IMPRESSIONS                                  
         BNE   ODMS01B                                                          
         CLC   =H'7',2(RF)         MIDAGE                                       
         BE    ODMS01B                                                          
         CLC   =H'15',2(RF)        REACH                                        
         BE    ODMS01B                                                          
         MVC   WORK+8(8),0(R2)     DEMO AMOUNT                                  
         CLI   PRECOPT,C'Y'        CABLE PRECISION                              
         BE    *+8                                                              
         CLI   1(RF),C'H'          CABLE IMPRESSIONS                            
         BNE   *+10                                                             
         MP    WORK(16),=PL2'10'   INCREASE BY 10 TIMES                         
         MP    WORK(16),=PL2'10'                                                
         DP    WORK(16),8(8,R2)    ROUND BY THE WEIGHT                          
         MVC   WORK+8(8),WORK                                                   
         XC    WORK(8),WORK                                                     
         AP    WORK(16),=PL3'5000'                                              
         DP    WORK(16),=PL8'10000'                                             
         MVC   0(8,R2),WORK                                                     
         B     ODMS06                                                           
ODMS01B  CLI   1(RF),C'H'          CABLE IMPRESSIONS                            
         BNE   ODMS01C                                                          
         AP    WORK(16),=PL2'50'                                                
         DP    WORK(16),=PL8'100'                                               
         MVC   0(8,R2),WORK                                                     
         B     ODMS06                                                           
ODMS01C  CP    8(8,R2),=PL8'0'     CHECK FOR ZERO DEMO                          
         BE    ODMSX                                                            
*                                                                               
         CLI   1(RF),C'R'          ADJUST FOR REACH RATING (RREACH)             
         BNE   *+20                                                             
         CLC   =H'15',2(RF)        REACH                                        
         BNE   *+10                                                             
         MP    WORK(16),=PL3'1000'                                              
*                                                                               
         CLI   1(RF),C'V'          ADJUST FOR VPH                               
         BE    *+12                                                             
         CLI   1(RF),C'M'          AND GAA VPH                                  
         BNE   *+10                                                             
         MP    WORK(16),=PL3'1000'                                              
*                                                                               
         CLI   1(RF),C'D'          ADJUST FOR AUD COMP                          
         BNE   *+10                                                             
         MP    WORK(16),=PL3'100'                                               
*                                                                               
         CLI   PRECOPT,C'Y'        CABLE PRECISION                              
         BNE   ODMS02A                                                          
         BRAS  RE,SYZ              CK IF COMPUTING SHARE AS S=Y/Z               
         BE    ODMS01F                                                          
         CLI   1(RF),C'L'                                                       
         BE    *+12                                                             
         CLI   1(RF),C'R'                                                       
         BNE   ODMS02A                                                          
ODMS01F  MP    WORK(16),=PL3'10'   INCREASE PREC TO 2DEC                        
         B     ODMS02A                                                          
*                                                                               
ODMS02   CP    8(8,R2),=PL8'0'     CHECK FOR ZERO DEMO                          
         BE    ODMSX                                                            
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)     DEMO AMOUNT                                  
                                                                                
ODMS02A  DP    WORK(16),8(8,R2)    DIVIDE BY THE WEIGHT                         
         MVC   0(8,R2),WORK                                                     
*                                                                               
*--CHECK REMAINDER FOR ROUNDING                                                 
         XC    WORK(8),WORK                                                     
         MP    WORK(16),=PL1'2'                                                 
         CP    WORK(16),8(8,R2)                                                 
         BL    *+10                                                             
         AP    0(8,R2),=PL1'1'                                                  
         B     ODMS06                                                           
*                                                                               
ODMS04   CP    8(8,R2),=PL8'0'     CHECK FOR ZERO DEMO                          
         BE    ODMSX                                                            
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)     DEMO AMOUNT                                  
         MP    WORK(16),=PL2'10'                                                
         DP    WORK(16),8(8,R2)    ROUND BY THE WEIGHT                          
         MVC   0(8,R2),WORK                                                     
*                                                                               
*--CHECK REMAINDER FOR ROUNDING                                                 
         XC    WORK(8),WORK                                                     
         MP    WORK(16),=PL1'2'                                                 
         CP    WORK(16),8(8,R2)                                                 
         BL    *+10                                                             
         AP    0(8,R2),=PL1'1'                                                  
         XC    WORK(16),WORK                                                    
         MVC   WORK+8(8),0(R2)                                                  
         AP    WORK(16),=PL2'50'                                                
         DP    WORK(16),=PL8'100'                                               
         MP    WORK(8),=PL2'10'                                                 
         MVC   0(8,R2),WORK                                                     
*                                                                               
ODMS06   MVI   GLHOOK,GLIDID       TELL DRIVER YOU JUST DID THIS                
         B     ODMSX                                                            
*                                                                               
ODMS08   L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         CLI   DROLTYP,C'N'        NO PRINT - NOT INTERESTED                    
         BE    ODMSX                                                            
         DROP  R1                                                               
*                                                                               
         CLI   GLARGS,0                                                         
         BNE   ODMS10                                                           
         CP    0(8,R2),MAXOPT      CHECK DEMO ABOVE THE MAXIMUM                 
         BH    ONOPRINT                                                         
         CP    0(8,R2),MINOPT      CHECK DEMO BELOW THE MINIMUM                 
         BL    ONOPRINT                                                         
         CLI   SOLOOPT,C'N'                                                     
         BNE   *+14                                                             
         CLC   24(4,R2),=F'1'      DONT PRINT SINGLE RUN SHOWS                  
         BE    ONOPRINT                                                         
*                                                                               
ODMS10   ZIC   RE,GLARGS          DEMO OFFSET                                   
         ZIC   R1,PODDMENT        NUMBER OF DEMOS IN A SERIES                   
         MH    R1,LNDEMCOD                                                      
         LA    RF,PODDEMO                                                       
         CLI   GLARGS,0                                                         
         BE    *+10                                                             
         AR    RF,R1                                                            
         BCT   RE,*-2                                                           
*                                                                               
         BRAS  RE,EDITIT                                                        
         BE    ODMSX               DID WE GET ONE?  YES, LEAVE                  
         CLI   PRECOPT,C'Y'                                                     
         BE    ODMS12                                                           
         EDIT  (P8,(R2)),(8,(R3)),ZERO=BLANK                                    
         B     ODMSX                                                            
*                                                                               
ODMS12   EDIT  (P8,(R2)),(8,(R3)),1,ZERO=BLANK                                  
         B     ODMSX                                                            
*                                                                               
ODMSX    DS    0H                                                               
         J     XIT                                                              
         DROP  R4                                                               
*                                                                               
ONOPRINT MVI   PRTSW,C'N'        NO PRINT                                       
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF COMPUTING SHARE AS S=Y/Z                                             
***********************************************************************         
SYZ      NTR1  BASE=*,LABEL=*                                                   
         CLI   1(RF),C'S'                                                       
         BNE   SYZNO                                                            
*                                                                               
         L     R4,PDADEMTB         SET UP DEMO BUFFER                           
         USING PDDMBUFF,R4                                                      
*                                                                               
         ZIC   RE,GLARGS          CHECK 'Y' IS THE DIVIDEND                     
         ZIC   R1,PODDMENT                                                      
         MH    R1,LNDEMCOD                                                      
         ICM   RF,15,APDDEML1                                                   
         BZ    SYZNO                                                            
         CLI   GLARGS,0                                                         
         BE    *+10                                                             
         AR    RF,R1                                                            
         BCT   RE,*-2                                                           
         CLI   1(RF),C'Y'                                                       
         BNE   SYZNO                                                            
*                                                                               
         ZIC   RE,GLARGS          CHECK 'Z' IS THE DIVISOR                      
         ZIC   R1,PODDMENT                                                      
         MH    R1,LNDEMCOD                                                      
         ICM   RF,15,APDDEML2                                                   
         BZ    SYZNO                                                            
         CLI   GLARGS,0                                                         
         BE    *+10                                                             
         AR    RF,R1                                                            
         BCT   RE,*-2                                                           
         CLI   1(RF),C'Z'                                                       
         BNE   SYZNO                                                            
*                                                                               
SYZYES   CR    RE,RE                                                            
         B     SYZX                                                             
SYZNO    CHI   RB,0                                                             
SYZX     J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF COMPUTING SHARE AS S=Y/Z FOR STACKED DEMOS                           
***********************************************************************         
STSYZ    NTR1  BASE=*,LABEL=*                                                   
         CLI   1(R4),C'S'                                                       
         BNE   STSYZNO                                                          
*                                                                               
         L     RE,PDADEMTB         SET UP DEMO BUFFER                           
         USING PDDMBUFF,RE                                                      
*                                                                               
         ICM   R5,15,APDDEML1      CHECK 'Y' IS THE DIVIDEND                    
         BZ    STSYZNO                                                          
         LA    R5,3(R5)                                                         
         ZIC   RF,GLARGS           DEMO OFFSET                                  
         ZIC   R1,PODDMENT         NUMBER OF DEMOS IN A SERIES                  
         CLI   PDOVSYS,3                                                        
         BNE   *+8                                                              
         LA    R5,1(R5)            ONE FOR FIRST DEMO                           
         MH    R1,LNDEMCOD                                                      
         CLI   GLARGS,0                                                         
         BE    *+10                                                             
         AR    R5,R1                                                            
         BCT   RF,*-2                                                           
*                                                                               
         LA    RF,PDSTADEF         START OF STACK MODIFIERS                     
         LR    R1,R4               A(CURRENT STACK MODIFIER)                    
         SR    R1,RF               DSPLACEMENT TO CURRENT STACK MODIFR          
         SRL   R1,1                /2    EA MODIFIER TAKES 2 BYTES              
         MH    R1,LNDEMCOD         * LENGTH OF DEMO EXPRESSION                  
         AR    R5,R1               + A(START OF DEMOS FOR THIS GROUP)           
         CLI   1(R5),C'Y'          CHECK 'Y' IS THE DIVIDEND                    
         BNE   STSYZNO                                                          
*                                                                               
         ICM   R5,15,APDDEML2      CHECK 'Z' IS THE DIVISOR                     
         BZ    STSYZNO                                                          
         LA    R5,3(R5)                                                         
         ZIC   RF,GLARGS           DEMO OFFSET                                  
         ZIC   R1,PODDMENT         NUMBER OF DEMOS IN A SERIES                  
         CLI   PDOVSYS,3                                                        
         BNE   *+8                                                              
         LA    R5,1(R5)            ONE FOR FIRST DEMO                           
         MH    R1,LNDEMCOD                                                      
         CLI   GLARGS,0                                                         
         BE    *+10                                                             
         AR    R5,R1                                                            
         BCT   RF,*-2                                                           
*                                                                               
         LA    RF,PDSTADEF         START OF STACK MODIFIERS                     
         LR    R1,R4               A(CURRENT STACK MODIFIER)                    
         SR    R1,RF               DSPLACEMENT TO CURRENT STACK MODIFR          
         SRL   R1,1                /2    EA MODIFIER TAKES 2 BYTES              
         MH    R1,LNDEMCOD         * LENGTH OF DEMO EXPRESSION                  
         AR    R5,R1               + A(START OF DEMOS FOR THIS GROUP)           
         CLI   1(R5),C'Z'          CHECK 'Z' IS THE DIVISOR                     
         BNE   STSYZNO                                                          
*                                                                               
         DROP  RE                                                               
STSYZYES CR    RB,RB                                                            
         B     STSYZX                                                           
STSYZNO  CHI   RB,0                                                             
STSYZX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* EDIT A DEMO VALUE FOR OUTPUT                                                  
**********************************************************************          
EDITIT   NTR1  BASE=*,LABEL=*                                                   
         LR    R5,RF               SAVE RF                                      
         L     R4,=A(PRECTABL)                                                  
         MVC   HALF(1),1(R5)         SET MODIFIER                               
*                                                                               
         OC    1(2,R5),1(R5)       COMSCORE DEMO?                               
         JNZ   EDIT02                                                           
         L     R4,=A(COMPREC)                                                   
         MVI   HALF,C'I'           DEFAULT IMPRESSIONS                          
         L     RF,APDNTDMS         COMSCORE DEMO NAMES                          
         ZIC   R1,3(R5)            DEMO OFFSET                                  
         SHI   R1,1                                                             
         MH    R1,=H'8'                                                         
         AR    RF,R1                                                            
         CLI   0(RF),C'X'          IMPRESSIONS?                                 
         BE    EDIT06                                                           
         MVC   HALF(1),0(RF)                                                    
         B     EDIT06                                                           
*                                                                               
         USING PRECTDST,R4                                                      
EDIT02   DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    EDITNEQ                                                          
         CLC   PRECSRCE,PODBFIL    ARE WE USING THIS SOURCE?                    
         BNE   EDIT04              NO, GO TO NEXT SOURCE                        
         L     R4,PRECADDR         LOAD ADDRESS OF PRECISION TABLE              
         B     EDIT06                                                           
*                                                                               
EDIT04   LA    R4,PRECTLEN(R4)                                                  
         B     EDIT02                                                           
         DROP  R4                                                               
         USING PDDMBUFF,R4                                                      
*                                                                               
EDIT06   DS    0H                                                               
         CLI   0(R4),X'FF'         ARE WE FINISHED?                             
         BE    EDITNEQ             YES, COULDN'T FIND IT                        
*        CLC   0(1,R4),1(R5)       GO THROUGH MODIFIERS                         
         CLC   0(1,R4),HALF        GO THROUGH MODIFIERS                         
         BE    EDIT08              FOUND IT, USE THE DATA                       
         LA    R4,PRECLEN(R4)                                                   
         B     EDIT06                                                           
*                                                                               
EDIT08   XC    EDTBLK,EDTBLK                                                    
         LA    R5,EDTBLK                                                        
         USING EBLOCK,R5                                                        
         ST    R2,EBAIN                                                         
         MVI   EBLIN,8                                                          
         MVI   EBTIN,C'P'                                                       
         ST    R3,EBAOUT                                                        
         CLI   PRECOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R4,3(R4)                                                         
         MVI   EBLOUT,8                                                         
         MVC   EBSCIN,1(R4)                                                     
         MVC   EBSCOUT,2(R4)                                                    
         MVC   EBDECS,3(R4)                                                     
         GOTO1 =V(EDITOR),DMCB,EBLOCK                                           
         B     EDITEQ                                                           
*                                                                               
EDITNEQ  LA    R1,1                                                             
         B     *+6                                                              
*                                                                               
EDITEQ   SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PUT DEMO DESCRIPTIONS TO HEADINGS                                             
***********************************************************************         
PUTHDEM  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R3                                                            
         L     R4,PDADEMTB         SET UP DEMO BUFFER                           
         USING PDDMBUFF,R4                                                      
         CLI   DRH1LITL,0          CHECK OVERRIDE ON HEADLINE 1                 
         BE    HDMS02                                                           
         LA    R3,198(R3)                                                       
         B     HDMS20                                                           
*                                                                               
HDMS02   ZIC   RE,GLARGS          DEMO OFFSET                                   
         LA    RF,PDDMNAME                                                      
         LA    R5,PODDEMO                                                       
         CLI   GLARGS,0                                                         
         BE    HDMS06                                                           
*                                                                               
HDMS04   LA    RF,12(RF)                                                        
         AH    R5,LNDEMCOD                                                      
         BCT   RE,HDMS04                                                        
*                                                                               
HDMS06   CLC   DUB(6),=CL6'GSTACK'                                              
         BNE   HDMS10                                                           
         LR    RE,RF               START OF DESCRIPTION                         
         LA    R1,6                MAX LENGTH-1                                 
*                                                                               
         TM    0(RE),X'F0'         A NUMBER?                                    
         BO    HDMS08              YES                                          
         LA    RE,1(RE)            NO,GET NEXT                                  
         BCT   R1,*-12             DECRMENT LENGTH                              
*                                                                               
         LR    RE,RF               NO NUMBERS, ALL LETTERS                      
         LA    R1,6                MAX LENGTH-1                                 
                                                                                
HDMS08   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(RE)                                                    
         LA    R3,198(R3)                                                       
         B     HDMS20                                                           
*                                                                               
HDMS10   MVC   1(7,R3),0(RF)                                                    
*                                                                               
         CLC   =C'REN',PDSOURCE    FOR RENTRAK DEMOS ARE                        
         BNE   HDMS12               'HOUSEHOLDS WITH'                           
         CLC   =C'AD',1(R3)                                                     
         BNE   *+10                                                             
         MVC   0(3,R3),=C'HHA'                                                  
         CLC   =C'WM',1(R3)                                                     
         BNE   *+10                                                             
         MVC   0(3,R3),=C'HHW'                                                  
         CLC   =C'MN',1(R3)                                                     
         BNE   *+10                                                             
         MVC   0(3,R3),=C'HHM'                                                  
*                                                                               
HDMS12   LA    R3,198(R3)                                                       
         CLC   DUB(5),=CL5'STACK'  CHECK IF STACK HEADLINE                      
         BNE   HDMS20                                                           
*                                                                               
         CLI   PDSTADEF+1,X'C1'    SEE IF NTI DEMO                              
         BNH   HDMS20              IF YES ONLY 1 LINE                           
         CLI   PODNADBK,X'FF'      NAD?                                         
         BE    HDMS32                                                           
         B     HDMS46              CHECK FOR FILTER LINE                        
*                                                                               
*                                                                               
*--SECOND LINE OF HEADING                                                       
*                                                                               
HDMS20   CLI   DRH2LITL,0          CHECK OVERRIDE ON HEADLINE 2                 
         BNE   HDMS30                                                           
         CLC   =C'IAG',PDSOURCE    FOR IAG USE CUSTOM HEADERS                   
         BNE   HDMS21                                                           
         CLI   0(R5),175                                                        
         BE    HDMS20A                                                          
         CLI   0(R5),176                                                        
         BE    HDMS20A                                                          
         CLI   0(R5),177                                                        
         BNE   HDMS21                                                           
HDMS20A  MVC   0(8,R3),=C' FACTORS'                                             
         B     HDMS30                                                           
*                                                                               
HDMS21   LA    R1,NADUHED          GET READY FOR 2ND HEADING OVERRIDE           
         CLI   PODNADBK,X'FF'      NAD?                                         
         BNE   HDMS26              NO                                           
         CLI   PDBKTYP,C'U'        YES, USER MEDIUM?                            
         BNE   HDMS26              NO                                           
*                                                                               
HDMS22   CLI   0(R1),X'FF'         YES, GET HEADING FROM TABLE                  
         BE    HDMS30                                                           
         CLC   0(1,R1),1(R5)                                                    
         BE    HDMS24                                                           
         LA    R1,3(R1)                                                         
         B     HDMS22                                                           
*                                                                               
HDMS24   MVC   5(2,R3),1(R1)                                                    
         B     HDMS30                                                           
*                                                                               
HDMS26   CLC   1(6,RF),=C'MIDAGE'  SUPPRESS LINE IF MIDAGE                      
         BE    *+10                                                             
         CLC   1(4,RF),=C'MAGE'                                                 
         BE    *+10                                                             
         MVC   2(5,R3),7(RF)       OTHERWISE MOVE IT IN                         
*                                                                               
* ADD 2-CHAR PERSONAL LANGUAGE TO HEADER                                        
HDMS28   GOTO1 DEMOCON,DMCB,0,('DEMOCON_14',WORK)                               
         ICM   RE,15,WORK+12       A(PERSONAL LANG. PRINTABLE DESCRIPS)         
         USING PLDD_DESC_TABLED,RE                                              
*                                                                               
         LA    RF,PODDEWPL         LIST OF 1-CHAR PERSONAL LANGUAGES            
         ZIC   R0,GLARGS                                                        
         MHI   R0,PODPLLQ                                                       
         AR    RF,R0               A(1-CHAR PERS LANG FOR CURRENT DEMO)         
*                                                                               
HDMS28A  CLC   =X'FFFF',0(RE)                                                   
         BE    HDMS30              NOT A PERSONAL LANGUAGE DEMO                 
         CLC   PLDD_INPUT_CHAR,0(RF)                                            
         BE    HDMS28B             MATCH ON 1-CHAR PERONAL LANGUAGE             
         LA    RE,PLDD_DESC_LENQ(RE)                                            
         B     HDMS28A                                                          
HDMS28B  MVC   WORK(5),2(R3)       MOVE MODIFIER DESCRPTN TO THE RIGHT          
         MVC   3(5,R3),WORK                                                     
         MVC   0(2,R3),PLDD_OUTPUT_2  DISPLAY 2-CHAR PERS LANG DESCRPTN         
         MVI   2(R3),C'/'                                                       
         DROP  RE                                                               
*                                                                               
*--THIRD LINE OF HEADING                                                        
*                                                                               
HDMS30   LA    R3,198(R3)                                                       
*                                                                               
HDMS32   CLI   DRH3LITL,0          CHECK OVERRIDE ON HEADLINE 3                 
         BNE   HDMS46                                                           
         CLI   0(R5),171           IS THIS FORETEL?                             
         BE    *+12                YES                                          
         CLI   0(R5),172           IS THIS OPTIMA?                              
         BNE   HDMS38              NO                                           
         CLI   PODNADBK,X'FF'      WAS NAD REQUESTED                            
         BE    HDMS38              YES                                          
         CLC   =C'NHW',PODBOOK+1   EXCEPTION                                    
         BE    HDMS38                                                           
         CLC   =C'HPW',PODBOOK+1   EXCEPTION                                    
         BE    HDMS38                                                           
         CLI   PDBKTYP,C'U'        IS THIS USER MEDIUM?                         
         BE    HDMS38              YES, SKIP THIRD LINE                         
         CLC   DUB(5),=CL5'STACK'  NAD DEMOS STACKS GET 2 LINES                 
         BE    HDMS38              CHECK FOR FILTER LINE                        
*                                                                               
         LA    R1,NADUHED          GET TABLE FOR 3RD HEADLINE                   
*                                                                               
HDMS34   CLI   0(R1),X'FF'         YES, GET HEADING FROM TABLE                  
         BE    HDMS38                                                           
         CLC   0(1,R1),0(R5)                                                    
         BE    HDMS36                                                           
         LA    R1,3(R1)                                                         
         B     HDMS34                                                           
*                                                                               
HDMS36   MVC   5(2,R3),1(R1)                                                    
*                                                                               
*                                                                               
HDMS38   CLI   0(R5),WB1TCRF       GET NAME FOR TCAR                            
         BL    HDMS39                                                           
         CLI   0(R5),WB1TCRL                                                    
         BL    HDMS39A                                                          
*                                                                               
HDMS39   DS    0H                                                               
         CLC   =C'NHW',PODBOOK+1   EXCEPTION                                    
         BE    HDMS39A                                                          
         CLC   =C'HPW',PODBOOK+1   EXCEPTION                                    
         BE    HDMS39A                                                          
         CLI   0(R5),175           ALLOW IAG MARKET BREAKS                      
         BE    HDMS39A                                                          
         CLI   0(R5),176                                                        
         BE    HDMS39A                                                          
         CLI   0(R5),177                                                        
         BE    HDMS39A                                                          
         CLI   PODNADBK,X'FF'      WAS NAD REQUESTED                            
         BNE   HDMS46              CHECK FOR FILTER LINE                        
HDMS39A  CLI   PDBKTYP,C'U'        IS THIS USER MEDIUM?                         
         BE    HDMS46              YES, SKIP THIRD LINE                         
         CLC   DUB(5),=CL5'STACK'  NAD DEMOS STACKS GET 2 LINES                 
         BNE   HDMS40              CHECK FOR FILTER LINE                        
         CLI   PDSTADEF+1,X'C1'                                                 
         BNH   HDMS46                                                           
*                                                                               
         LA    R5,PODDEMO          SAVE IT JUST IN CASE                         
         SR    R0,R0                                                            
         ZIC   RE,GLARGS                                                        
         ZIC   R1,PODDMENT                                                      
         MH    R1,LNDEMCOD                                                      
         MR    R0,RE                                                            
         AR    R5,R1                                                            
*                                                                               
HDMS40   L     RE,=A(PFXTAB)                                                    
*                                                                               
HDMS42   CLI   0(RE),X'FF'                                                      
         BNE   *+8                                                              
         B     HDMS46              CHECK FOR FILTER LINE                        
         CLC   0(1,R5),0(RE)                                                    
         BE    HDMS44                                                           
         LA    RE,8(RE)                                                         
         B     HDMS42                                                           
*                                                                               
HDMS44   MVC   1(7,R3),1(RE)                                                    
         MVC   DMCB+4(4),=X'D9000A01' GET CENTER                                
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R3),8                                                 
*                                                                               
HDMS46   LR    R3,R2                                                            
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
* INPUT AVG FLAG                                                                
**********************************************************************          
IAVGQR   NTR1  BASE=*,LABEL=*                                                   
         CLI   PDAVGTYP,X'FF'                                                   
         BNE   *+10                                                             
         MVC   0(4,R2),=C'N/A '                                                 
         CLI   PDAVGTYP,X'00'                                                   
         BNE   *+10                                                             
         MVC   0(4,R2),=C'PROG'                                                 
         CLI   PDAVGTYP,X'01'                                                   
         BNE   *+10                                                             
         MVC   0(4,R2),=C'TRAK'                                                 
         CLI   PDAVGTYP,X'02'                                                   
         BNE   *+10                                                             
         MVC   0(4,R2),=C'SOLO'                                                 
         J     XIT                                                              
         LTORG                                                                  
**********************************************************************          
* INPUT BOOK ROUTINE                                                            
**********************************************************************          
IBOOKR   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,PDSBOOK                                                       
*                                                                               
         MVC   SVPDBOOK,PDSBOOK    SAVE ORIGINAL BOOK FORMAT                    
         CLI   PDNOWEB,C'Y'        FOR ESTIMATED BOOKS                          
         BNE   *+8                                                              
         NI    PDSBOOK+1,X'7F'     TURN OFF HIGH ORDER BIT                      
*                                                                               
         CLI   GLARGS,C'Y'         YEARLY FORMAT                                
         BNE   IBOOK02                                                          
         MVI   0(R2),0             ZERO MONTH ON OUTPUT                         
         EDIT  (1,(R5)),(2,2(R2)),ZERO=BLANK   MOVE YEAR                        
         B     IBOOK40                                                          
*                                                                               
IBOOK02  CLC   =C'NAW',PDSOURCE    WEEKLY BOOKS                                 
         BE    IBOOK08                                                          
         CLC   =C'CNAW',PDSOURCE                                                
         BE    IBOOK08                                                          
         CLC   =C'OPA ',PDSOURCE                                                
         BE    IBOOK08                                                          
         CLC   =C'OTP ',PDSOURCE                                                
         BE    IBOOK08                                                          
         CLC   =C'COM ',PDSOURCE                                                
         BE    IBOOK08                                                          
         OC    PDCSSTA,PDCSSTA     COMSCORE NETWORK?                            
         JNZ   IBOOK08                                                          
         CLC   PDFILE(3),=CL3'NAD' MONTHLY BOOKS                                
         BE    IBOOK26                                                          
         CLC   PDFILE(3),=CL3'MPA'                                              
         BE    IBOOK26                                                          
         CLC   PDFILE(3),=CL3'PAV'                                              
         BE    IBOOK26                                                          
         CLC   PDFILE(3),=CL3'CTP'  COUNTY COVG IS MONTHLY                      
         BE    IBOOK26                                                          
*                                                                               
         CLC   =C'CSI',PDSOURCE    CSI IS WEEKLY AFTER 96                       
         BE    IBOOK04                                                          
         CLC   =C'BBM',PDSOURCE                                                 
         BNE   IBOOK06                                                          
         CLI   PDBKTYP,C'W'                                                     
         BNE   IBOOK06                                                          
         CLI   SVBBMWK,C'Y'        IS THIS A M/D/Y REQUEST?                     
         BNE   IBOOK06             NO                                           
         B     IBOOK08                                                          
*                                                                               
IBOOK04  CLI   0(R5),95                                                         
         BH    IBOOK08                                                          
*                                                                               
IBOOK06  CLC   PDFILE(2),=CL3'TP'                                               
         BE    IBOOK26                                                          
*                                                                               
IBOOK08  CLC   PDFILE(2),=CL3'IUN'                                              
         BE    IBOOK26                                                          
*                                                                               
         CLI   GLARGS,C'W'         WEEKLY FORMAT                                
         BNE   IBOOK10                                                          
         CLC   PDFILE(3),=C'WTP'                                                
         BE    IBOOK08S                                                         
         MVC   0(2,R2),SVPDBOOK      KEEP ORIGINAL SORT ORDER                   
         GOTOR NETUNBK,DMCB,(C'W',PDSBOOK),DUB,GETDAY,ADDAY,GETBROAD            
         GOTOR DATCON,DMCB,DUB,(7,2(R2))                                        
         B     IBOOK40                                                          
* UNWEEK FOR NSI WEEKLY BOOKS                                                   
IBOOK08S MVC   0(2,R2),SVPDBOOK      KEEP ORIGINAL SORT ORDER                   
         MVC   DUB(2),PDSBOOK                                                   
         MVI   DUB+2,1                                                          
         GOTOR NSIWEEK,DMCB,(C'D',DUB),(0,GETDAY),ADDAY,DATCON                  
         ZICM  R1,DMCB+1,(7)                                                    
         MVC   DUB(6),0(R1)                                                     
         GOTOR DATCON,DMCB,(X'80',DUB),(7,2(R2))                                
         B     IBOOK40                                                          
*                                                                               
IBOOK10  CLI   GLARGS,C'M'         MONTHLY FORMAT                               
         BNE   IBOOK18                                                          
         CLI   PDCALOPT,PDCALNTI   MONTHLY FORMAT - NTI                         
         BE    IBOOK14                                                          
*                                                                               
IBOOK12  MVC   0(2,R2),SVPDBOOK    KEEP ORIGINAL SORT ORDER                     
         GOTOR NETUNBK,DMCB,(C'M',(R2)),DUB,GETDAY,ADDAY,GETBROAD               
         GOTOR DATCON,DMCB,DUB,(6,2(R2))                                        
         PACK  DUB(8),DUB+2(2)                                                  
         CVB   R1,DUB                                                           
         STC   R1,1(R2)            LOAD MONTH CODE                              
         B     IBOOK40                                                          
*                                                                               
* MONTHS FOR CALENDAR=NTI OPTION                                                
IBOOK14  GOTOR UNWEEK,DMCB,PDSBOOK,DUB                                          
         GOTOR DATCON,DMCB,(0,DUB),(3,FULL)                                     
         MVC   DUB(3),FULL                                                      
*                                  CALENDARIZE NTI MONTHS                       
         L     RF,ACOMFACS         GET A(MONTH/DATES TABLE)                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,MTHDATES                                               
         ICM   RE,15,0(R1)         A(MONTH/DATES TABLE)                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            LENGTH OF TABLE ENTRY                        
*                                                                               
         USING MTHDATD,RE                                                       
         CLC   DUB(3),MDSDATE      NOT IN TABLE                                 
         BL    IBOOK12                                                          
*                                                                               
IBOOK16  CLI   MDNLSMN,13          END OF TABLE                                 
         BE    IBOOK12                                                          
         CLC   DUB(3),MDEDATE                                                   
         BNH   *+10                                                             
         AR    RE,RF                                                            
         B     IBOOK16                                                          
         MVC   DUB(2),MDNLBOOK                                                  
         MVC   0(2,R2),MDNLBOOK                                                 
         DROP  RE                                                               
         GOTOR DATCON,DMCB,(3,DUB),(6,2(R2))                                    
         B     IBOOK40                                                          
*                                                                               
IBOOK18  CLI   PDCALOPT,PDCALNTI   QUARTERLY FORMAT - NTI                       
         BE    IBOOK22                                                          
*                                                                               
IBOOK20  MVC   0(2,R2),SVPDBOOK    KEEP ORIGINAL SORT ORDER                     
         GOTOR NETUNBK,DMCB,(C'Q',PDSBOOK),DUB,GETDAY,ADDAY,GETBROAD            
         CLI   DUB+3,C'9'          ADJUST FOR Y2K                               
         BNH   *+16                                                             
         IC    R0,DUB+3                                                         
         SHI   R0,10                                                            
         STC   R0,DUB+3                                                         
         MVC   2(5,R2),DUB                                                      
         MVC   DUB(1),DUB+1                                                     
         NI    DUB,X'0F'                                                        
         MVC   1(1,R2),DUB         QUARTER CODE                                 
         B     IBOOK40                                                          
*                                                                               
* QUARTERS FOR CALENDAR=NTI                                                     
IBOOK22  GOTOR UNWEEK,DMCB,PDSBOOK,DUB                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,NENTIQRT  GET NIELSEN QUARTER TABLE                    
         ICM   RE,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NEQTRD,RE                                                        
         CLC   DUB(6),NEQTRFRS     NOT IN TABLE                                 
         BL    IBOOK20                                                          
*                                                                               
IBOOK24  CLI   0(RE),X'FF'                                                      
         BE    IBOOK20                                                          
         CLC   DUB(6),NEQTRLST     FIND NTI QUARTER                             
         BNH   *+12                                                             
*** WHOEVER WORKS ON THIS MODULE NEXT: GET RID OF THIS REFERENCE TO             
*** NEQTRQ !!! USE THE SOFT LENGTH RETURNED FROM DEMTABS INSTEAD !!!            
         LA    RE,NEQTRQ(RE)                                                    
         B     IBOOK24                                                          
*                                                                               
         MVC   2(2,R2),NEQTRQT                                                  
         MVI   4(R2),C'/'                                                       
         MVC   5(2,R2),NEQTRYR                                                  
         MVC   1(1,R2),NEQTRQT+1   SET BINARY QUARTER                           
         NI    1(R2),X'0F'                                                      
*                                                                               
         PACK  DUB(8),NEQTRYR                                                   
         CVB   RE,DUB                                                           
         CH    RE,=H'27'                                                        
         BH    *+8                                                              
         LA    RE,100(RE)                                                       
         STC   RE,0(R2)            SET BINARY YEAR                              
         B     IBOOK40                                                          
         DROP  RE                                                               
*                                                                               
*-----FOR NAD OUTPUT                                                            
IBOOK26  CLI   GLARGS,C'Q'         MONTHLY FORMAT                               
         BNE   IBOOK34                                                          
         LA    RE,NADQUART                                                      
         LA    R4,4                                                             
*                                                                               
IBOOK28  CLC   1(1,R5),5(RE)                                                    
         BL    IBOOK30                                                          
         CLC   1(1,R5),6(RE)                                                    
         BNH   IBOOK32                                                          
         CLC   1(1,R5),7(RE)                                                    
         BE    IBOOK32                                                          
         CLC   1(1,R5),8(RE)                                                    
         BE    IBOOK32                                                          
*                                                                               
IBOOK30  LA    RE,9(RE)                                                         
         BCT   R4,IBOOK28                                                       
         DC    H'0'                                                             
*                                                                               
IBOOK32  MVC   0(1,R2),PDSBOOK     MOVE START YEAR IN                           
         MVC   1(1,R2),4(RE)       MOVE TABLE CODE                              
         LR    R4,R2                                                            
         MVC   2(3,R4),0(RE)       MOVE LITERAL                                 
         LA    R4,5(R4)                                                         
         EDIT  (1,(R5)),(2,(R4)),ZERO=BLANK   MOVE YEAR                         
         B     IBOOK40                                                          
*                                                                               
*BOOK34  MVC   0(2,R2),PDSBOOK                                                  
IBOOK34  MVC   0(2,R2),SVPDBOOK    KEEP ORIGINAL SORT ORDER                     
         XC    DUB,DUB                                                          
         MVC   DUB(2),PDSBOOK                                                   
         GOTOR DATCON,DMCB,(3,DUB),(9,2(R2))                                    
         CLC   =C'IUN',PDFILE                                                   
         BNE   IBOOK38                                                          
         L     RF,=A(SVCLST)                                                    
*                                                                               
IBOOK36  CLI   0(RF),X'FF'         DEFAULT - JUST LEAVE ALONE                   
         BE    IBOOK40                                                          
         CLC   PDSTYPE,2(RF)       FOUND A MATCH                                
         BE    *+12                                                             
         LA    RF,L'SVCLST(RF)                                                  
         B     IBOOK36                                                          
*                                                                               
         MVC   5(2,R2),6(R2)       SHOW BOOK TYPE                               
         MVC   7(1,R2),1(RF)       SET INV KEY SOURCE                           
         B     IBOOK40                                                          
*                                                                               
IBOOK38  CLI   PDNOWEB,C'Y'                                                     
         BNE   IBOOK40                                                          
         MVC   5(2,R2),6(R2)       SHOW BOOK IS ESTIMATED                       
         MVI   7(R2),C'E'                                                       
         B     IBOOK40                                                          
*                                                                               
*-----REMOVE YEAR TO MERGE QUARTERS AND MONTHS                                  
IBOOK40  CLI   GLARGS+2,C'A'                                                    
         BNE   IBOOK42                                                          
         MVI   0(R2),0                                                          
         LA    R2,4(R2)                                                         
         CLI   GLARGS,C'M'                                                      
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         MVC   0(3,R2),=3XL1'40'                                                
*                                                                               
*--CHECK FOR REVERSE DATE SEQUENCE                                              
IBOOK42  TM    PDINSEQ,PDSEQBK                                                  
         BZ    IBOOK44                                                          
         ZIC   RE,PDIBKSEQ                                                      
         SLL   RE,2                                                             
         A     RE,APODOBSQ                                                      
         MVI   0(RE),C'B'                                                       
         MVC   1(1,RE),PDIBKSEQ    GET INPUT SEQUENCE                           
         MVC   2(2,RE),0(R2)       SAVE ACTUAL BOOK                             
         MVC   0(2,R2),0(RE)       RESEQUENCE                                   
*                                                                               
IBOOK44  CLI   GLARGS+1,C'R'                                                    
         BNE   *+10                                                             
         XC    0(8,R2),XFF                                                      
         MVC   PDSBOOK,SVPDBOOK    RESTORE ORIG BOOK FORMT,JUST IN CASE         
IBOOKRX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
FILTSRTB DC    X'01',CL5'NAD  ',CL5'     '                                      
         DC    X'02',CL5'NAD-T',CL5'     '                                      
         DC    X'03',CL5'NAD-D',CL5'     '                                      
         DC    X'04',CL5'NTI  ',CL5'NTI-N'                                      
         DC    X'05',CL5'NTI-T',CL5'     '                                      
         DC    X'06',CL5'PIV  ',CL5'     '                                      
         DC    X'07',CL5'NSI-W',CL5'     '                                      
         DC    X'08',CL5'MPA  ',CL5'MPA-N'                                      
         DC    X'09',CL5'EMI  ',CL5'     '                                      
         DC    X'0A',CL5'NMI  ',CL5'     '                                      
         DC    X'0B',CL5'TP   ',CL5'TP -N'                                      
         DC    X'0C',CL5'PAV  ',CL5'PAV-N'                                      
         DC    X'0D',CL5'DPT  ',CL5'DPT-N'                                      
         DC    X'0E',CL5'MPA-A',CL5'     '                                      
         DC    X'0F',CL5'TP -A',CL5' '                                          
         DC    X'10',CL5'PAV-A',CL5' '                                          
         DC    X'11',CL5'DPT-A',CL5' '                                          
         DC    X'12',CL5'CSI  ',CL5' '                                          
         DC    X'13',CL5'BBM  ',CL5' '                                          
         DC    X'14',CL5'T4   ',CL5'T4 -N'                                      
         DC    X'15',CL5'T4 -A',CL5' '                                          
         DC    X'16',CL5'SRC  ',CL5' '                                          
         DC    X'17',CL5'NHI-T',CL5'NHT-H'                                      
         DC    X'17',CL5'NHT-H',CL5' '                                          
         DC    X'18',CL5'NHT  ',CL5' '                                          
         DC    X'19',CL5'NHT-D',CL5' '                                          
         DC    X'1A',CL5'NHT-T',CL5' '                                          
         DC    X'1B',CL5'NSI  ',CL5'TP -N'                                      
         DC    X'1C',CL5'ARB  ',CL5'TP -A'                                      
         DC    X'1D',CL5'NHW-H',CL5' '                                          
         DC    X'1E',CL5'NHW  ',CL5' '                                          
         DC    X'1F',CL5'T3   ',CL5'T3-C '                                      
         DC    X'20',CL5'T3-B ',CL5' '                                          
         DC    X'21',CL5'IUN  ',CL5'IUN-N'                                      
         DC    X'22',CL5'TP   ',CL5'TP -M'                                      
         DC    X'22',CL5'MFX  ',CL5'TP -M'                                      
         DC    X'23',CL5'PAV  ',CL5'PAV-M'                                      
         DC    X'24',CL5'T4 -M',CL5' '                                          
         DC    X'25',CL5'IUN-S',CL5' '                                          
         DC    X'26',CL5'IUN-M',CL5' '                                          
         DC    X'27',CL5'NAW  ',CL5' '                                          
         DC    X'28',CL5'NAW-D',CL5' '                                          
         DC    X'29',CL5'NAW-T',CL5' '                                          
         DC    X'2A',CL5'NHW-D',CL5' '                                          
         DC    X'2B',CL5'HWH-T',CL5' '                                          
         DC    X'2C',CL5'SQAD ',CL5' '                                          
         DC    X'2D',CL5'TCAR ',CL5' '                                          
         DC    X'2D',CL5'WB1  ',CL5' '                                          
         DC    X'2E',CL5'RADAR',CL5' '                                          
         DC    X'2F',CL5'NHC-T',CL5' '                                          
         DC    X'2F',CL5'NHC-H',CL5' '                                          
         DC    X'30',CL5'NHC  ',CL5' '                                          
         DC    X'31',CL5'NCOS ',CL5' '                                          
         DC    X'32',CL5'OPI  ',CL5' '                                          
         DC    X'33',CL5'CNAD ',CL5' '                                          
         DC    X'34',CL5'CNADT',CL5' '                                          
         DC    X'35',CL5'CNAW ',CL5' '                                          
         DC    X'36',CL5'CNAWT',CL5' '                                          
         DC    X'37',CL5'ACM  ',CL5' '                                          
         DC    X'38',CL5'HPM  ',CL5' '                                          
         DC    X'39',CL5'HPM-D',CL5' '                                          
         DC    X'3A',CL5'HPM-H',CL5' '                                          
         DC    X'3B',CL5'HPM-T',CL5' '                                          
         DC    X'3C',CL5'HPW  ',CL5' '                                          
         DC    X'3D',CL5'HPW-D',CL5' '                                          
         DC    X'3E',CL5'HPW-H',CL5' '                                          
         DC    X'3F',CL5'HPW-T',CL5' '                                          
         DC    X'40',CL5'HPC  ',CL5' '                                          
         DC    X'41',CL5'HPC-H',CL5' '                                          
         DC    X'42',CL5'HPC-T',CL5' '                                          
         DC    X'43',CL5'ACMWB',CL5' '                                          
         DC    X'44',CL5'MXM  ',CL5' '                                          
         DC    X'45',CL5'COM  ',CL5' '                                          
         DC    X'FF'                                                            
         EJECT                                                                  
ROUTLIST DS    0F                  ROUTINE ADDRESS LIST                         
         DC    C'INPG    ',A(INPG)       PROGRAM                                
         DC    C'IPRGG   ',A(IPRGG)      PROGRAM GROUP                          
         DC    C'IPGN    ',A(IPGN)       PROGRAM NAME FROM GROUP                
         DC    C'IEPI    ',A(IEPI)       EPISODE                                
         DC    C'IDDSEPI ',A(IDDSEPI)    DDS ONLY EPISODE                       
         DC    C'IFIL    ',A(IFIL)       FILTER                                 
         DC    C'IVIEWT  ',A(IVIEWT)     VIEWING TYPE                           
         DC    C'IBKOUT  ',A(IBKOUT)     BREAKOUT INDICATOR                     
         DC    C'IAIRT   ',A(IAIRT)      AIRING TYPE                            
         DC    C'ICONT   ',A(ICONT)      CONTENT TYPE                           
         DC    C'IWEK    ',A(IWEK)       WEEK                                   
         DC    C'INTI    ',A(INTI)       NTI CODE                               
         DC    C'ILNTI   ',A(ILNTI)      CALL + NTI CODE                        
         DC    C'IAFFIL  ',A(IAFFIL)     TP/NSI PROGRAM NET AFFILIATES          
         DC    C'IUFFIL  ',A(IUFFIL)     TP/NSI USER AFFILIATES                 
         DC    C'ICOVER  ',A(ICOVER)     COVERAGE (NTI)                         
         DC    C'ISCOUNT ',A(ISCOUNT)    STATION COUNT (NTI)                    
         DC    C'IDAY    ',A(IDAY)       DAY                                    
         DC    C'IADAY   ',A(IADAY)      ACTIVE DAYS                            
         DC    C'ITIM    ',A(ITIM)       TIME                                   
         DC    C'IETIME  ',A(IETIME)     END TIME (COMSCORE)                    
         DC    C'ISTIME  ',A(ISTIME)     START TIME (COMSCORE)                  
         DC    C'IMILS   ',A(IMILS)      MILITARY START TIME                    
         DC    C'IMILE   ',A(IMILE)      MILITARY END TIME                      
         DC    C'INET    ',A(INET)       NETWORK                                
         DC    C'ICNET   ',A(ICNET)      COMSCORE NETWORK                       
         DC    C'INETNUM ',A(INETNUM)    COMSCORE NETWORK NUMBER                
         DC    C'ILNET   ',A(ILNET)      LONG NETWORK NAME                      
         DC    C'IFILE   ',A(IFILE)      FILE                                   
         DC    C'ISRCE   ',A(ISRCE)      SOURCE                                 
         DC    C'IMRKT   ',A(IMRKT)      MARKET                                 
         DC    C'IMRKTNAM',A(IMRKTNAM)   MARKET NAME                            
         DC    C'IMRKTALF',A(IMRKTALF)   ALPHA MARKET CODE                      
         DC    C'IPTYP   ',A(IPTYP)      PROGRAM TYPE (2 CHAR)                  
         DC    C'IPTYPL  ',A(IPTYPL)     PROGRAM TYPE (4 CHAR)                  
         DC    C'IPTYPS  ',A(IPTYPS)     PROGRAM TYPE (SUB)                     
         DC    C'IPGNEW  ',A(IPGNEW)     PROGRAM RECD NEW INDICATOR             
         DC    C'IPGTIER ',A(IPGTIER)    PROGRAM RECORD TIER                    
         DC    C'IPGRAT  ',A(IPGRAT)     PROGRAM RECORD CONTENT RATING          
         DC    C'ICOMPUTE',A(ICOMPUTE)   COMPUTE SUMS                           
         DC    C'IBOOK   ',A(IBOOK)      BOOK                                   
         DC    C'IDEM    ',A(IDEM)       DEMOS                                  
         DC    C'IDPNAME ',A(IDPNAME)    IN-HOUSE DAYPART NAME                  
         DC    C'IDPT    ',A(IDPT)       DAYPART                                
         DC    C'IFDT    ',A(IFDT)       FIRST AIR DATE                         
         DC    C'ILDT    ',A(ILDT)       LAST AIR DATE                          
         DC    C'IRUN    ',A(IRUN)       TIMES AIRED                            
         DC    C'IWT     ',A(IWT)        TOTAL WEIGHT                           
         DC    C'IDURTOT ',A(IDURTOT)    TOTAL DURATION                         
         DC    C'IMPDOL  ',A(IMPDOL)     MONITOR PLUS DOLLARS                   
         DC    C'IMPDOLO ',A(IMPDOLO)    OLD MPLUS DOLLAR ROUTINE               
         DC    C'IDUR    ',A(IDUR)       DURATION                               
         DC    C'IRDT    ',A(IRDT)       RUN DATE                               
         DC    C'IDEMAIR ',A(IDEMAIR)    DEMO/TIMES AIRED                       
         DC    C'IDEMRPT ',A(IDEMRPT)    DEMO/REPEAT FLAG                       
         DC    C'ISTDATA ',A(ISTDATA)    STACK DATA                             
         DC    C'ISTDEM  ',A(ISTDEM)     STACK DEMOS                            
         DC    C'ISTAT   ',A(ISTAT)      STATION CALL LETTERS                   
         DC    C'ISSTA   ',A(ISTAT)      SHORT CALL LETTERS (4 CHAR)            
         DC    C'IPURE   ',A(IPURE)      PURE NUMBER                            
         DC    C'ISIU    ',A(ISIU)       PURE NUMBER + EFF DATE                 
         DC    C'IQHDUR  ',A(IQHDUR)     QUARTER HOUR DURATION                  
         DC    C'IPERIOD ',A(IPERIOD)    SID PERIOD                             
         DC    C'IPRGTYP ',A(IPRGTYP)    SID PROGRAM TYPE EXPANSION             
         DC    C'IPSOURCE',A(IPSOUR)     NSI PROGRAMMING SOURCE                 
         DC    C'IEFFDAT ',A(IEFFDAT)    SID EFFECTIVE DATE                     
         DC    C'IWEIGHT ',A(IWEIGHT)    WEIGHT                                 
         DC    C'IDPRG   ',A(IDPRG)      DAILY PROGRAM NAME                     
         DC    C'IGRPID  ',A(IGRPID)     MARKET GROUP ID                        
         DC    C'ICOST   ',A(ICOST)      COST(SID)                              
         DC    C'IGAA    ',A(IGAA)       GAA FLAG                               
         DC    C'IORIGIN ',A(IORIGIN)    ORIGINATING STATION(MPA/NPD)           
         DC    C'IAVGQ   ',A(IAVGQ)      AVERGAGE TYPE (AVG?)                   
         DC    C'IPREM   ',A(IPREM)      PREMIERE FLAG                          
         DC    C'ITEXT   ',A(ITEXT)      TEXT FIELD                             
         DC    C'ITRACK  ',A(ITRAK)      TRACKAGE NAME                          
         DC    C'ITRNUM  ',A(ITRNUM)     TRACKAGE NUMBER                        
         DC    C'ITLNUM  ',A(ITLNUM)     TELECAST NUMBER                        
         DC    C'ICPROG  ',A(ICPROG)     CABLE MODIFIED PROGRAM NAME            
         DC    C'IDAILY  ',A(IDAILY)     FLAG FOR NTI OVERNITES                 
         DC    C'INTILG  ',A(INTILG)     LONG NTI CODE                          
         DC    C'ISQART  ',A(ISQART)     SQAD QUARTER                           
         DC    C'IFEED   ',A(IFEED)      NETWORK FEED                           
         DC    C'ICOMM   ',A(ICOMM)      COMMERCIAL STATUS                      
         DC    C'ICTYNAM ',A(ICTYNAM)    COUNTY NAME                            
         DC    C'ICTYNO  ',A(ICTYNO)     COUNTY NUMBER                          
         DC    C'IDMANO  ',A(IDMANO)     DMA NUMBER                             
         DC    C'IDMAORG ',A(IDMAORG)    DMA OF ORIGIN                          
         DC    C'IDMANAM ',A(IDMANAM)    DMA NAME                               
         DC    C'ISTATE  ',A(ISTATE)     STATE                                  
         DC    C'ISTANAM ',A(ISTANAM)    STATE NAME                             
         DC    C'ISYSC   ',A(ISYSC)      SYSCODE                                
         DC    C'IUYEAR  ',A(IUYEAR)     UNIVERSE YEAR                          
         DC    C'ILIVE   ',A(ILIVE)      LIVE INDICATOR                         
         DC    C'IRSTIM  ',A(IRSTIM)     ROUNDED START TIME                     
         DC    C'IAE     ',A(IAE)        ROUNDED START TIME                     
         DC    C'ILDDAT  ',A(ILDDAT)     LOAD DATE                              
         DC    C'IIVTYP  ',A(IIVTYP)     INPUT VIEWING TYPE                     
         DC    C'ICDUR   ',A(ICDUR)      COMMERCIAL DURATION                    
         DC    C'ICTCAST ',A(ICTCAST)    NUM OF COMMERCIAL TELECASTS            
         DC    C'IPVS    ',A(IPVS)       PROGRAM VIEWING SOURCE& TYPE           
         DC    C'INADCDUR',A(INADCDUR)   COMMERCIAL DURATION                    
         DC    C'IPRMIN  ',A(IPRMIN)     PROGRAM MINUTE                         
         DC    C'ICOMMF  ',A(ICOMMF)     COMMERCIAL FLAG                        
         DC    C'IPROMOF ',A(IPROMOF)    PROMO FLAG                             
         DC    C'IPSAF   ',A(IPSAF)      PSA FLAG                               
         DC    C'IPGPF   ',A(IPGPF)      PROGRAM+ FLAG                          
         DC    C'ISECCOM ',A(ISECCOM)    COMMERCIAL SECONDS                     
         DC    C'ISECPRO ',A(ISECPRO)    PROMO SECONDS                          
         DC    C'ISECPSA ',A(ISECPSA)    PSA SECONDS                            
         DC    C'ISECPGP ',A(ISECPGP)    PROGRAM+ SECONDS                       
         DC    C'ITSECCOM',A(ITSECCOM)   COMMERCIAL+PROMO+PSA SECONDS           
         DC    C'IPDSTIM ',A(IPDSTIM)    POD START TIME                         
         DC    C'IPDETIM ',A(IPDETIM)    POD END TIME                           
         DC    C'IPDLEN  ',A(IPDLEN)     POD LENGTH IN MINUTES                  
         DC    C'IPDNUM  ',A(IPDNUM)     POD NUMBER                             
         DC    C'I%COMM  ',A(IPCOMM)     % COMMERCIAL SECONDS                   
         DC    C'I%PROMO ',A(IPPROMO)    % PROMO SECONDS                        
         DC    C'I%TCOMM ',A(IPTCOMM)    % TOTAL COMMERCIAL SECONDS             
         DC    C'I%PROG+ ',A(IPPROGP)    % PROGRAM+ SECONDS                     
         DC    C'IMOP    ',A(IMOP)       MINUTE OF PROGRAM                      
         DC    C'ITIMEX  ',A(ITIMEX)     EXACT TIME FOR MXM                     
         DC    C'IAVGPODL',A(IAVGPODL)   AVERAGE POD LENGTH                     
         DC    C'IGAPPED ',A(IGAPPED)    GAPPED PROGRAM INDICATOR               
         DC    C'ICORDAT ',A(ICORDAT)    CORRECTION DATE                        
         DC    C'ICORTYP ',A(ICORTYP)    CORRECTION TYPE                        
         DC    C'ISECDUR ',A(ISECDUR)    TOTAL SECOND DURATION                  
         DC    C'IWKDAT  ',A(IWKDAT)     WEEK DATE                              
         DC    C'IOS     ',A(IOS)        ORDERED SUSTAINER INDICATOR            
         DC    C'IRPT    ',A(IRPT)       IS REPEAT AIRING                       
         DC    C'ISPREM  ',A(ISPREM)     IS SEASON PREMIERE                     
         DC    C'ISPEC   ',A(ISPEC)      IS SPECIAL AIRING                      
         DC    C'IEPNUM  ',A(IEPNUM)     EPISODE NUMBER                         
*                                                                               
         DC    C'ONPG    ',A(ONPG)       PROGRAM                                
         DC    C'OPRGG   ',A(OPRGG)      PROGRAM GROUP                          
         DC    C'OPGN    ',A(ONPG)       PROGRAM NAME FROM GROUP                
         DC    C'OEPI    ',A(OEPI)       EPISODE                                
         DC    C'ODDSEPI ',A(ODDSEPI)    DDS ONLY EPISODE                       
         DC    C'OFIL    ',A(OFIL)       FILTER                                 
         DC    C'OVIEWT  ',A(OVIEWT)     VIEWING TYPE                           
         DC    C'OBKOUT  ',A(OBKOUT)     BREAKOUT INDICATOR                     
         DC    C'OAIRT   ',A(OAIRT)      AIRING TYPE                            
         DC    C'OCONT   ',A(OCONT)      CONTENT TYPE                           
         DC    C'OWEK    ',A(OWEK)       WEEK                                   
         DC    C'ONTI    ',A(ONTI)       NTI CODE                               
         DC    C'OLNTI   ',A(OLNTI)      CALL + NTI CODE                        
         DC    C'OAFFIL  ',A(OAFFIL)     TP/NSI PROGRAM NET AFFILIATES          
         DC    C'OUFFIL  ',A(OUFFIL)     TP/NSI USER AFFILIATES                 
         DC    C'OCOVER  ',A(OCOVER)     COVERAGE                               
         DC    C'OSCOUNT ',A(OSCOUNT)    STATION COUNT                          
         DC    C'ODAY    ',A(ODAY)       DAY                                    
         DC    C'OADAY   ',A(OADAY)      ACTIVE DAYS                            
         DC    C'OTIM    ',A(OTIM)       TIME                                   
         DC    C'OETIME  ',A(OETIME)     END TIME (COMSCORE)                    
         DC    C'OSTIME  ',A(OSTIME)     START TIME (COMSCORE)                  
         DC    C'OMILS   ',A(OMILS)      MILITARY START TIME                    
         DC    C'OMILE   ',A(OMILE)      MILITARY END TIME                      
         DC    C'ONET    ',A(ONET)       NETWORK                                
         DC    C'OCNET   ',A(OCNET)      COMSCORE NETWORK                       
         DC    C'ONETNUM ',A(ONETNUM)    COMSCORE NETWORK NUMBER                
         DC    C'OLNET   ',A(OLNET)      NETWORK                                
         DC    C'OFILE   ',A(OFILE)      FILE                                   
         DC    C'OSRCE   ',A(OSRCE)      SOURCE                                 
         DC    C'OMRKT   ',A(OMRKT)      MARKET                                 
         DC    C'OMRKTNAM',A(OMRKTNAM)   MARKET NAME                            
         DC    C'OMRKTALF',A(OMRKTALF)   ALPHA MARKET CODE                      
         DC    C'OORIGIN ',A(OORIGIN)    ORIGINATING STATION                    
         DC    C'OPTYP   ',A(OPTYP)      PROGRAM TYPE                           
         DC    C'OPTYPL  ',A(OPTYPL)     PROGRAM TYPE (4 CHAR)                  
         DC    C'OPTYPS  ',A(OPTYPS)     PROGRAM TYPE (SUB)                     
         DC    C'OPGNEW  ',A(OPGNEW)     PROGRAM RECORD NEW INDICATOR           
         DC    C'OPGTIER ',A(OPGTIER)    PROGRAM RECORD TIER                    
         DC    C'OPGRAT  ',A(OPGRAT)     PROGRAM RECORD CONTENT RATING          
         DC    C'OBOOK   ',A(OBOOK)      BOOK                                   
         DC    C'ODEM    ',A(ODEM)       DEMOS                                  
         DC    C'ODPT    ',A(ODPT)       DAYPART                                
         DC    C'ORUN    ',A(ORUN)       TIMES AIRED                            
         DC    C'OWT     ',A(OWT)        TOTAL WEIGHT                           
         DC    C'ODURTOT ',A(ODURTOT)    TOTAL DURATION                         
         DC    C'OMPDOL  ',A(OMPDOL)     MONITOR PLUS DOLLARS                   
         DC    C'OMPDOLO ',A(OMPDOLO)    OLD MPLUS DOLLARS ROUTINE              
         DC    C'ORDT    ',A(ORDT)       RUN DATE                               
         DC    C'ODATE   ',A(ODATE)      DATE                                   
         DC    C'ODEMAIR ',A(ODEMAIR)    DEMO/TIMES AIRED                       
         DC    C'ODEMRPT ',A(ODEMRPT)    DEMO/REPEAT FLAG                       
         DC    C'OSTDATA ',A(OSTDATA)    STACK DATA                             
         DC    C'OSTDEM  ',A(OSTDEM)     STACK DEMOS                            
         DC    C'OSTAT   ',A(OSTAT)      STATION CALL LETTERS                   
         DC    C'OSSTA   ',A(OSTAT)      SHORT CALL LETTERS (4 CHAR)            
         DC    C'OPURE   ',A(OPURE)      PURE NUMBER                            
         DC    C'OPURE3  ',A(OPURE3)     PURE NUMBER 3 CHARACTERS               
         DC    C'OSIU    ',A(OSIU)       PURE NUMBER + EFF DATE                 
         DC    C'OQHDUR  ',A(OQHDUR)     QUARTER HOUR DURATION                  
         DC    C'OPERIOD ',A(OPERIOD)    SID PERIOD                             
         DC    C'OPRGTYP ',A(OPRGTYP)    SID PROGRAM TYPE EXPANSION             
         DC    C'OPSOURCE',A(OPSOUR)     NSI PROGRAMMING SOURCE                 
         DC    C'OEFFDAT ',A(OEFFDAT)    SID EFFECTIVE DATE                     
         DC    C'OWEIGHT ',A(OWEIGHT)    WEIGHT                                 
         DC    C'ODPRG   ',A(ODPRG)      DAILY PROGRAM NAME                     
         DC    C'OGRPID  ',A(OGRPID)     MARKET GROUP ID                        
         DC    C'OCOST   ',A(OCOST)      COST(SID)                              
         DC    C'OGAA    ',A(OGAA)       GAA FLAG                               
         DC    C'OAVGQ   ',A(OAVGQ)      AVERAGE TYPE (AVG?)                    
         DC    C'OPREM   ',A(OPREM)      PREMIERE FLAG                          
         DC    C'OTRACK  ',A(OTRAK)      TRACKAGE NAME                          
         DC    C'OTRNUM  ',A(OTRNUM)     TRACKAGE NUMBER                        
         DC    C'OTLNUM  ',A(OTLNUM)     TELECAST NUMBER                        
         DC    C'OCPROG  ',A(OCPROG)     CABLE MODIFIED PROGRAM NAME            
         DC    C'ODAILY  ',A(ODAILY)     FLAG FOR NTI OVERNITES                 
         DC    C'ONTILG  ',A(ONTILG)     LONG NTI CODE                          
         DC    C'OSQART  ',A(OSQART)     SQAD QUARTER                           
         DC    C'OFEED   ',A(OFEED)      NETWORK FEED                           
         DC    C'OCOMM   ',A(OCOMM)      COMMERCIAL STATUS                      
         DC    C'OCTYNAM ',A(OCTYNAM)    COUNTY NAME                            
         DC    C'OCTYNO  ',A(OCTYNO)     COUNTY NUMBER                          
         DC    C'ODMANO  ',A(ODMANO)     DMA NUMBER                             
         DC    C'ODMAORG ',A(ODMAORG)    DMA OF ORIGIN                          
         DC    C'ODMANAM ',A(ODMANAM)    DMA NAME                               
         DC    C'OSTATE  ',A(OSTATE)     STATE                                  
         DC    C'OSTANAM ',A(OSTANAM)    STATE NAME                             
         DC    C'OSYSC   ',A(OSYSC)      SYSCODE                                
         DC    C'OUYEAR  ',A(OUYEAR)     UNIVERSE YEAR                          
         DC    C'OLIVE   ',A(OLIVE)      LIVE INDICATOR                         
         DC    C'ORSTIM  ',A(ORSTIM)     ROUNDED START TIME                     
         DC    C'OAE     ',A(OAE)        ROUNDED START TIME                     
         DC    C'OLDDAT  ',A(OLDDAT)     LOAD DATE                              
         DC    C'OIVTYP  ',A(OIVTYP)     INPUT VIEWING TYPE                     
         DC    C'OCDUR   ',A(OCDUR)      COMMERCIAL DURATION                    
         DC    C'OCTCAST ',A(OCTCAST)    NUM OF COMMERCIAL TELECASTS            
         DC    C'OPVS    ',A(OPVS)       PROGRAM VIEWING SOURCE & TYPE          
         DC    C'ONADCDUR',A(ONADCDUR)   COMMERCIAL DURATION                    
         DC    C'OPRMIN  ',A(OPRMIN)     PROGRAM MINUTE                         
         DC    C'OCOMMF  ',A(OCOMMF)     COMMERCIAL FLAG                        
         DC    C'OPROMOF ',A(OPROMOF)    PROMO FLAG                             
         DC    C'OPSAF   ',A(OPSAF)      PSA FLAG                               
         DC    C'OPGPF   ',A(OPGPF)      PSA FLAG                               
         DC    C'O%COMM  ',A(OPCOMM)     % COMMERCIAL SECONDS                   
         DC    C'O%PROMO ',A(OPPROMO)    % PROMO SECONDS                        
         DC    C'OMOP    ',A(OMOP)       MINUTE OF PROGRAM                      
         DC    C'OTIMEX  ',A(OTIMEX)     EXACT TIME FOR MXM                     
         DC    C'OAVGPODL',A(OAVGPODL)   AVERAGE POD LENGTH                     
         DC    C'OGAPPED ',A(OGAPPED)    GAPPED PROGRAM INDICATOR               
         DC    C'OCORDAT ',A(OCORDAT)    CORRECTION DATE                        
         DC    C'OCORTYP ',A(OCORTYP)    CORRECTION TYPE                        
         DC    C'OOS     ',A(OOS)        ORDERED SUSTAINER INDICATOR            
         DC    C'ORPT    ',A(OYESNO)     IS REPEAT AIRING                       
         DC    C'OSPREM  ',A(OYESNO)     IS SEASON PREMIERE                     
         DC    C'OSPEC   ',A(OYESNO)     IS SPECIAL AIRING                      
         DC    C'OEPNUM  ',A(OEPNUM)     EPISODE NUMBER                         
*                                                                               
         DC    C'HDEMAIR ',A(HDEMAIR)    DEMO/TIMES AIRED                       
         DC    C'HDEMRPT ',A(HDEMRPT)    DEMO/REPEAT FLAG                       
         DC    C'HDEM    ',A(HDEM)       DEMO HEADLINE                          
         DC    C'HSTDEM  ',A(HSTDEM)     STACK DEMO HEADLINE                    
         DC    C'HFILTER ',A(HFILTER)    FILTER INFORMATION                     
         DC    C'OPDTIME ',A(OPDTIME)    POD START/END TIME                     
         DC    C'OPDLEN  ',A(OPDLEN)     POD LENGTH IN MINUTES                  
         DC    C'OPDNUM  ',A(OPDNUM)     POD NUMBER                             
         DC    X'FF'                                                            
         EJECT                                                                  
DAYTAB   DC    0XL5                                                             
         DC    XL1'00',CL3'M-F',X'7C'                                           
         DC    XL1'01',CL3'MON',X'40'                                           
         DC    XL1'02',CL3'TUE',X'20'                                           
         DC    XL1'03',CL3'WED',X'10'                                           
         DC    XL1'04',CL3'THU',X'08'                                           
         DC    XL1'05',CL3'FRI',X'04'                                           
         DC    XL1'06',CL3'SAT',X'02'                                           
         DC    XL1'07',CL3'SUN',X'01'                                           
         DC    XL1'08',CL3'M-S',X'7F'                                           
*                                                                               
*-REP SYSTEM DEFINES                                                            
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
NOSTDLST DC    AL1(234),XL7'00000000000000'                                     
         DC    AL1(235),CL7'INDX   '                                            
         DC    AL1(236),CL7'DIFF   '                                            
*-DEMOS                                                                         
         DC    XL1'C9',CL7'IMP    '                                             
         DC    XL1'E3',CL7'IMP    '                                             
         DC    XL1'D7',CL7'PUT    '                                             
         DC    XL1'D9',CL7'RTG    '                                             
         DC    XL1'E2',CL7'SHR    '                                             
         DC    XL1'E5',CL7'VPH    '                                             
         DC    XL1'C4',CL7'(COM)  '                                             
         DC    XL1'C5',CL7'E-RTG  '                                             
         DC    XL1'E4',CL7'(UNI)  '                                             
         DC    XL1'D8',CL7'TPSHR  '                                             
         DC    XL1'D6',CL7'TPPUT  '                                             
         DC    XL1'D3',CL7'GARTG  '                                             
         DC    XL1'D4',CL7'GAVPH  '                                             
         DC    XL1'D5',CL7'GAIMP  '                                             
         DC    XL1'E7',CL7'TSH    '                                             
         DC    XL1'C1',CL7'VWR    '                                             
         DC    XL1'F0',CL7'DEMO   '                                             
         DC    XL1'F1',CL7'SHARE  '                                             
         DC    XL1'F2',CL7'HPT    '                                             
         DC    XL1'C3',CL7'C/RTG  '     C'C'                                    
         DC    XL1'E8',CL7'IBASE  '     C'Y'                                    
         DC    XL1'E9',CL7'PBASE  '     C'Z'                                    
*-NAD DEFINES                                                                   
         PRINT GEN                                                              
       ++INCLUDE DENADCATS                                                      
         PRINT NOGEN                                                            
         DC    XL1'FF'                                                          
         EJECT                                                                  
GENTABLE DC    CL1'L',CL7'LISTEN '                                              
         DC    CL1'G',CL7'GIRLS  '                                              
         DC    CL1'F',CL7'FEMALE '                                              
         DC    CL1'W',CL7'WOMEN  '                                              
         DC    CL1'B',CL7'BOYS   '                                              
         DC    CL1'M',CL7'MEN    '                                              
         DC    CL1'V',CL7'VIEWERS'                                              
         DC    CL1'A',CL7'ADULTS '                                              
         DC    X'FF'                                                            
         EJECT                                                                  
* SPGENSTAB                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENSTAB                                                      
         PRINT ON                                                               
* DRGLOBAL                                                                      
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
         PRINT ON                                                               
* DRIVETABLE                                                                    
         PRINT OFF                                                              
       ++INCLUDE DRIVETABLE                                                     
         PRINT ON                                                               
* DRINTRECD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DRINTRECD                                                      
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* SPGENREP                                                                      
         PRINT OFF                                                              
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEPODWORK                                                      
         EJECT                                                                  
       ++INCLUDE NEPODBOOK                                                      
         EJECT                                                                  
* SPGENAGY                                                                      
         PRINT OFF                                                              
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
       ++INCLUDE DDEBLOCK                                                       
       ++INCLUDE DEDBEXTRAD                                                     
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
PRECTDST DSECT                                                                  
PRECSRCE DS    CL3                                                              
         DS    CL1                 FOR ALIGNMENT                                
PRECADDR DS    F                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102NEPODDRIVE09/17/20'                                      
         END                                                                    
